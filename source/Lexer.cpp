#include "Lexer.hpp"

#include <cstring>
#include <format>
#include <functional>
#include <istream>
#include <string>

namespace GenericStudyCompiler {
    bool Lexer::LoadNextLine(Context& context) {
        if (std::getline(*this->stream, this->buffer)) {
            context.str = this->buffer.c_str();
            context.range.end = 0;
            this->cursor.y++;
            return true;
        }

        return false;
    }

    void Lexer::HandleStringLiteral(Context& context) {
        std::string buffer;

        if (context.range.start != context.range.end) {
            std::unordered_map<std::string_view, std::function<void ()>> cases = {
                { "R", [this, &context, &buffer]() {
                    std::string delimiter;
                    std::size_t delimiterLength;

                    do {
                        context.cache = context.str[++context.range.end];

                        if (std::isspace(context.cache))
                            throw std::runtime_error("Invalid space in raw string literal delimiter");


                        if ('\0' == context.cache)
                            throw std::runtime_error("Unexpected end of raw string literal delimiter");

                        if ('(' == context.cache)
                            break;

                        delimiter.push_back(context.cache);
                    } while (1);

                    delimiterLength = delimiter.length();
                    context.range.end++;

                    do {
                        do {
                            context.cache = context.str[context.range.end];

                            if ('\0' == context.cache) {
                                buffer.append(context.str + context.range.start, context.range.end - context.range.start).push_back('\n');

                                if (this->LoadNextLine(context)) {
                                    context.range.start = 0;
                                    continue;
                                }

                                throw std::runtime_error("Unterminated raw string literal: missing closing delimiter");
                            }

                            context.range.end++;
                        } while (')' != context.cache);

                        if (strnlen(context.str + context.range.end, delimiterLength) < delimiterLength) {
                            buffer.append(context.str + context.range.start, context.range.end - context.range.start).push_back('\n');

                            if (this->LoadNextLine(context)) {
                                context.range.start = 0;
                                continue;
                            }

                            throw std::runtime_error("Unterminated raw string literal: missing closing delimiter");
                        }

                        if (std::memcmp(context.str + context.range.end, delimiter.data(), delimiterLength))
                            continue;

                        context.range.end += delimiterLength;

                        if ('\"' == context.str[context.range.end])
                            break;
                    } while (1);

                    context.range.end++;
                    buffer.append(context.str + context.range.start, context.range.end - context.range.start);
                } }
            };

            buffer = std::string(context.str + context.range.start, context.range.end - context.range.start);
            std::unordered_map<std::string_view, std::function<void ()>>::iterator _case = cases.find(buffer);
            buffer.clear();

            if (_case != cases.end()) {
                _case->second();
            } else throw std::runtime_error("Unrecognized string literal format");
        } else {
            std::size_t accumulator;
            std::int32_t index;
            bool escaping = false;
            bool running = true;

            buffer.push_back('\"');

            do {
                context.cache = context.str[++context.range.end];

                switch (context.cache) {
                    case '\0':
                        throw std::runtime_error("Unterminated string literal");

                    case '\"':
                        if (escaping) {
                            buffer.push_back('\"');
                            escaping = false;
                        } else {
                            running = false;
                        }

                        break;

                    case '\'':
                        buffer.push_back('\'');
                        escaping = false;

                        break;

                    case '0': case '1': case '2': case '3':
                    case '4': case '5': case '6': case '7':
                        if (escaping) {
                            accumulator = context.cache - '0';

                            for (index = 0; index < 2; index++) {
                                context.cache = context.str[context.range.end + 1];

                                if (!IsOctogonal(context.cache))
                                    break;

                                context.range.end++;

                                accumulator = (accumulator << 3) + (context.cache - '0');
                            }

                            if (0xFF < accumulator)
                                throw std::runtime_error("Invalid octal escape sequence: exceeds ASCII range");

                            buffer.push_back(static_cast<char>(accumulator));
                            escaping = false;
                        } else {
                            buffer.push_back(context.cache);
                        }

                        break;

                    case '?':
                        buffer.push_back('?');
                        escaping = false;

                        break;

                    case '\\':
                        if (escaping) {
                            buffer.push_back('\\');
                            escaping = false;
                        } else {
                            escaping = true;
                        }

                        break;

                    case 'a':
                        if (escaping) {
                            buffer.push_back('\a');
                            escaping = false;
                        } else {
                            buffer.push_back('a');
                        }

                        break;

                    case 'b':
                        if (escaping) {
                            buffer.push_back('\b');
                            escaping = false;
                        } else {
                            buffer.push_back('b');
                        }

                        break;

                    case 'f':
                        if (escaping) {
                            buffer.push_back('\f');
                            escaping = false;
                        } else {
                            buffer.push_back('f');
                        }

                        break;

                    case 'n':
                        if (escaping) {
                            buffer.push_back('\n');
                            escaping = false;
                        } else {
                            buffer.push_back('n');
                        }

                        break;

                    case 'r':
                        if (escaping) {
                            buffer.push_back('\r');
                            escaping = false;
                        } else {
                            buffer.push_back('r');
                        }

                        break;

                    case 't':
                        if (escaping) {
                            buffer.push_back('\t');
                            escaping = false;
                        } else {
                            buffer.push_back('t');
                        }

                        break;

                    case 'v':
                        if (escaping) {
                            buffer.push_back('\v');
                            escaping = false;
                        } else {
                            buffer.push_back('v');
                        }

                        break;

                    case 'x':
                        if (escaping) {
                            accumulator = 0;

                            for (index = 0; index < 2; index++) {
                                if (!std::isxdigit(context.str[++context.range.end]))
                                    throw std::runtime_error("Incomplete hexadecimal escape sequence");
                                accumulator = (accumulator << 4) + (std::isdigit(context.cache) ? (context.cache - '0') : ((context.cache & 0b1011111) - ('A' - 10)));
                            }

                            buffer.push_back(static_cast<char>(accumulator));

                            escaping = false;
                        } else {
                            buffer.push_back('x');
                        }

                        break;

                    default:
                        if (escaping)
                            // TODO: improve this
                            throw std::runtime_error(std::format("Invalid escape character '{}'", context.cache));

                        buffer.push_back(context.cache);

                        break;
                }
            } while (running);

            context.range.end++;
            buffer.push_back('\"');
        }

        context.token.metadata.type = Token::Type::Literal;
        context.token.data = std::move(buffer);
    }

    void Lexer::HandleCharacterLiteral(Context& context) {
        std::string buffer;
        std::size_t accumulator = 0;
        buffer.push_back('\'');

        context.cache = context.str[++context.range.end];

        if ('\0' == context.cache)
            throw std::runtime_error("Unterminated character literal");

        if ('\\' == context.cache) {
            switch (context.cache = context.str[++context.range.end]) {
                case '\0':
                    throw std::runtime_error("Unterminated character literal");

                case '\"': buffer.push_back('\"'); break;
                case '\'': buffer.push_back('\''); break;
                case '?':  buffer.push_back('?');  break;
                case '\\': buffer.push_back('\\'); break;
                case 'a':  buffer.push_back('\a'); break;
                case 'b':  buffer.push_back('\b'); break;
                case 'f':  buffer.push_back('\f'); break;
                case 'n':  buffer.push_back('\n'); break;
                case 'r':  buffer.push_back('\r'); break;
                case 't':  buffer.push_back('\t'); break;
                case 'v':  buffer.push_back('\v'); break;

            case '0': case '1': case '2': case '3':
            case '4': case '5': case '6': case '7':
                accumulator = context.cache - '0';

                for (int i = 0; i < 2; ++i) {
                    context.cache = context.str[context.range.end + 1];
                    if (!IsOctogonal(context.cache)) break;
                    
                    context.range.end++;
                    accumulator = (accumulator << 3) + (context.cache - '0');
                }

                if (accumulator > 0xFF)
                    throw std::runtime_error("Invalid octal escape sequence: exceeds ASCII range"); // TODO: format this

                buffer.push_back(static_cast<char>(accumulator));

                break;

                case 'x':
                    accumulator = 0;

                    for (int i = 0; i < 2; ++i) {
                        context.cache = context.str[++context.range.end];

                        if (!std::isxdigit(context.cache))
                            throw std::runtime_error("Incomplete hexadecimal escape sequence");

                        accumulator = (accumulator << 4) + (std::isdigit(context.cache) ? (context.cache - '0') : (std::tolower(context.cache) - 'a' + 10));
                    }

                    buffer.push_back(static_cast<char>(accumulator));

                    break;

                default: {
                    buffer.push_back('\\');

                    do {
                        buffer.push_back(context.cache);

                        if ('\'' == (context.cache = context.str[++context.range.end]))
                            break;

                        if ('\0' == context.cache)
                            throw std::runtime_error("Invalid unclosored escape sequence: " + buffer);
                    } while (true);

                    context.range.end++;
                    buffer.push_back('\'');

                    throw std::runtime_error("Invalid escape sequence: " + buffer);
                }
            }
        } else
            buffer.push_back(context.cache);

        if ('\'' != context.str[++context.range.end])
            throw std::runtime_error("Unterminated character literal");

        context.range.end++;

        buffer.push_back('\'');

        context.token.data = std::move(buffer);
        context.token.metadata.type = Token::Type::Literal;
    }

    std::shared_ptr<PrefixTree> Lexer::GetKeywords() noexcept {
        return this->keywords;
    }

    std::shared_ptr<PrefixTree> Lexer::GetPunctuation() noexcept {
        return this->punctuation;
    }

    Lexer::Lexer(std::unique_ptr<std::istream>&& stream, std::shared_ptr<PrefixTree>&& keywords, std::shared_ptr<PrefixTree>&& punctuation) : buffer(), cursor{ 1, 1 }, keywords(std::move(keywords)), punctuation(std::move(punctuation)), stream(std::move(stream)) {
        if (!this->stream.get())
            throw std::invalid_argument("Invalid stream pointer"); // TODO: No such file or directory
        std::getline(*this->stream, this->buffer);
    }

    Lexer::Token Lexer::GetNextToken() {
        Context context;
        context.str = this->buffer.c_str();
        context.range.end = this->cursor.x - 1;

        try {
            do {
                while (std::isspace(context.cache = context.str[context.range.end]))
                    context.range.end++;

                context.cursor = {(context.range.start = context.range.end) + 1, this->cursor.y};

                if (std::isalpha(context.cache) || '_' == context.cache) {
                    do {
                        context.cache = context.str[++context.range.end];
                    } while (std::isalpha(context.cache) || '_' == context.cache);

                    if (std::isdigit(context.cache)) {
                        do {
                            context.cache = context.str[++context.range.end];
                        } while (std::isalnum(context.cache) || '_' == context.cache);

                        switch (context.cache) {
                            case '\"':
                                this->HandleStringLiteral(context);

                                break;

                            case '\'':
                                this->HandleCharacterLiteral(context);

                                break;

                            default:
                                context.token.metadata.type = Token::Type::Identifier;
                                context.token.data = std::string(context.str + context.range.start, context.range.end - context.range.start);

                                break;
                        }
                    } else switch (context.cache) {
                        case '\"':
                            this->HandleStringLiteral(context);

                            break;

                        case '\'':
                            this->HandleCharacterLiteral(context);

                            break;

                        default:
                            context.token.data = std::string(context.str + context.range.start, context.range.end - context.range.start);
                            context.token.metadata.type = this->keywords->Search(context.token.data) ? Token::Type::Keyword : Token::Type::Identifier;

                            break;
                    }
                } else if (std::isdigit(context.cache)) {
                    if ('0' == context.cache) {
                        switch (context.str[++context.range.end]) {
                            case 'B': case 'b':
                                do {
                                    context.cache = context.str[++context.range.end];
                                } while (IsBinary(context.cache));

                                break;

                            case 'X': case 'x':
                                do {
                                    context.cache = context.str[++context.range.end];
                                } while (std::isxdigit(context.cache));

                                break;

                            case '.':
                                if ('.' != context.str[context.range.end + 1]) {
                                    do {
                                        context.cache = context.str[++context.range.end];
                                    } while (std::isdigit(context.cache));

                                    switch (context.cache) {
                                        case 'F': case 'f':
                                        case 'L': case 'l':
                                            context.cache = context.str[++context.range.end];

                                        default:
                                            break;
                                    }
                                }

                                break;

                            case '0': case '1': case '2': case '3':
                            case '4': case '5': case '6': case '7':
                                do {
                                    context.cache = context.str[++context.range.end];
                                } while (IsOctogonal(context.cache));

                                if (!('8' == context.cache || '9' == context.cache))
                                    break;

                            case '8': case '9':
                                // TODO: improve
                                throw std::runtime_error("Invalid octogonal sequence");

                            default:
                                break;
                        }
                    } else {
                        do {
                            context.cache = context.str[++context.range.end];
                        } while (std::isdigit(context.cache));

                        if ('.' == context.cache) {
                            if ('.' != context.str[context.range.end + 1]) {
                                do {
                                    context.cache = context.str[++context.range.end];
                                } while (std::isdigit(context.cache));

                                switch (context.cache) {
                                    case 'F': case 'f':
                                    case 'L': case 'l':
                                        context.cache = context.str[++context.range.end];

                                    default:
                                        break;
                                }
                            }
                        }
                    }

                    if (std::isalnum(context.str[context.range.end])) { // Experimental
                        while (std::isalnum(context.str[++context.range.end]));

                        throw std::runtime_error("Invalid sequence"); // TODO: Imporve this
                    }

                    context.token.data = std::string(context.str + context.range.start, context.range.end - context.range.start);
                    context.token.metadata.type = Token::Type::Literal;
                } else if (std::ispunct(context.cache)) {
                    switch (context.cache) {
                        case '\"':
                            this->HandleStringLiteral(context);

                            break;

                        case '\'':
                            this->HandleCharacterLiteral(context);

                            break;

                        case '/':
                            context.cache = context.str[context.range.end + 1];

                            if ('/' == context.cache) {
                                if (LoadNextLine(context))
                                    continue;

                                context.token.metadata.type = Token::Type::EndOfFile;

                                break;
                            } else if ('*' == context.cache) {
                                context.range.end += 2;

                                do {
                                    do {
                                        context.cache = context.str[context.range.end];

                                        if ('\0' == context.cache) {
                                            if (LoadNextLine(context))
                                                continue;

                                            throw std::runtime_error("Unclosed multi-line comment.");
                                        }

                                        context.range.end++;
                                    } while ('*' != context.cache);
                                } while ('/' != context.str[context.range.end]);

                                context.range.end++;

                                continue;
                            }

                        default:
                            context.token.data = GreedyMatching(context.str + context.range.start, *this->punctuation);
                            context.range.end += context.token.data.length();

                            if (context.range.start != context.range.end) {
                                context.token.metadata.type = Token::Type::Punctuation;
                            } else {
                                do {
                                    context.cache = context.str[++context.range.end];

                                    if ('/' == context.cache) {
                                        context.cache = context.str[context.range.end + 1];

                                        if ('/' == context.cache) {
                                            break;
                                        } else if ('*' == context.cache) {
                                            break;
                                        }

                                        continue;
                                    }
                                } while (std::ispunct(context.cache));

                                context.token.metadata.type = Token::Type::Unknown;
                                context.token.data = std::string(context.str + context.range.start, context.range.end - context.range.start);
                            }

                            break;
                    }
                } else if ('\0' == context.cache) {
                    if (LoadNextLine(context))
                        continue;

                    context.token.metadata.type = Token::Type::EndOfFile;
                } else {
                    do {
                        context.cache = context.str[++context.range.end];
                    } while (!(std::isspace(context.cache) || '\0' == context.cache));

                    context.token.data = std::string(context.str + context.range.start, context.range.end - context.range.start);
                    context.token.metadata.type = Token::Type::Unknown;
                }

                break;
            } while (1);
        } catch (const std::exception& e) {
            context.token.data = e.what();
            context.token.metadata.type = Token::Type::Error;
        }

        this->cursor.x = context.range.end + 1;

        context.token.metadata.range = { context.cursor, this->cursor };

        return context.token;
    }
}