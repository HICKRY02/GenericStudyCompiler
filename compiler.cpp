#include <array>
#include <bitset>
#include <cassert>
#include <cctype>
#include <cstddef>
#include <cstring>
#include <exception>
#include <filesystem>
#include <format>
#include <fstream>
#include <functional>
#include <initializer_list>
#include <iostream>
#include <locale>
#include <memory>
#include <queue>
#include <sstream>
#include <stack>
#include <stdexcept>
#include <string>
#include <string_view>
#include <unordered_map>
#include <unordered_set>
#include <utility>

template<typename T>
union Point {
    using Type          = T;

    std::array<Type, 2> data;

    struct {
        Type                x;
        Type                y;
    };
};

template<typename T>
union Range {
    using Type          = T;

    std::array<Type, 2> data;

    struct {
        Type                start;
        Type                end;
    };

    struct {
        Type                offset;
        Type                size;
    };
};

bool IsBinary(char c) {
    return '0' == c || '1' == c;
}

bool IsOctogonal(char c) {
    return '0' <= c && '7' >= c;
}

class PrefixTree {
public:
    class Node {
        mutable std::unordered_map<char, Node>  children;
        bool                                    isEndOfString = false;

    public:
        bool IsEndOfString() const noexcept { return this->isEndOfString; }

        bool HasChildren() const noexcept { return !this->children.empty(); }

        void Insert(std::string_view str) {
            Node* current = this;

            for (char c : str)
                current = &current->children.try_emplace(c, Node()).first->second;

            current->isEndOfString = true;
        }

        Node* Search(char c) {
            std::unordered_map<char, Node>::iterator iterator = this->children.find(c);

            if (this->children.end() != iterator)
                return &iterator->second;

            return nullptr;
        }

        const Node* Search(char c) const {
            std::unordered_map<char, Node>::iterator iterator = this->children.find(c);

            if (this->children.end() != iterator)
                return &iterator->second;

            return nullptr;
        }
    };

private:
    Node root;

public:
    Node& GetRoot() noexcept {
        return this->root;
    }

    const Node& GetRoot() const noexcept {
        return this->root;
    }

    void Insert(std::string_view str) {
        root.Insert(str);
    }

    void Insert(std::initializer_list<std::string_view> list) {
        for (const auto& str : list)
            root.Insert(str);
    }

    bool Search(std::string_view str) const {
        const Node* current = &this->root;

        for (char c : str) {
            current = current->Search(c);

            if (!current)
                return false;
        }

        return current->IsEndOfString();
    }
};

std::string GreedyMatching(std::string_view str, const PrefixTree& pattern) {
    std::size_t longestMatch = 0;
    std::size_t i = 0;
    const PrefixTree::Node* current = &pattern.GetRoot();

    while (current->HasChildren()) {
        current = current->Search(str[i++]);

        if (!current)
            break;

        if (current->IsEndOfString())
            longestMatch = i;
    }

    return std::string(str.data(), longestMatch);
}

class Lexer {
public:
    struct Token {
        std::size_t identifier;
        std::string value;
    };

private:
    struct Context {
        char                cache;
        const char*         str;
        Point<std::size_t>  cursor;
        Range<std::size_t>  range;
        Token               token;
    };

    std::string                     buffer;
    Point<std::size_t>              cursor;
    std::shared_ptr<PrefixTree>     keywords;
    std::shared_ptr<PrefixTree>     punctuation;
    std::unique_ptr<std::istream>   stream;

    bool ItsOver(Context& context) {
        if (std::getline(*this->stream, this->buffer)) {
            context.str = this->buffer.c_str();
            context.range.end = 0;
            this->cursor.y++;
            return false;
        }

        return true;
    }

    void HandleStringLiteral(Context& context) {
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

                                if (this->ItsOver(context))
                                    throw std::runtime_error("Unterminated raw string literal: missing closing delimiter");

                                context.range.start = 0;
                                continue;
                            }

                            context.range.end++;
                        } while (')' != context.cache);
                        
                        if (strnlen(context.str + context.range.end, delimiterLength) < delimiterLength) {
                            buffer.append(context.str + context.range.start, context.range.end - context.range.start).push_back('\n');

                            if (this->ItsOver(context))
                                throw std::runtime_error("Unterminated raw string literal: missing closing delimiter");
                            
                            context.range.start = 0;
                            continue;
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
            } else {
                throw std::runtime_error("Unrecognized string literal format");
            }
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
                            // TODO:
                            throw std::runtime_error(std::format("Invalid escape character '{}'", context.cache));

                        buffer.push_back(context.cache);

                        break;
                }
            } while (running);

            context.range.end++;
            buffer.push_back('\"');
        }

        context.token.identifier = std::hash<std::string_view>{}("Literal");
        context.token.value = std::move(buffer);
    }

    void HandleCharacterLiteral(Context& context) {
        std::string buffer;
        std::size_t accumulator = 0;
        buffer.push_back('\'');

        context.cache = context.str[++context.range.end];

        if (context.cache == '\0')
            throw std::runtime_error("Unterminated character literal");

        if (context.cache == '\\') {
            context.cache = context.str[++context.range.end];

            switch (context.cache) {
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
                    throw std::runtime_error("Invalid octal escape sequence: exceeds ASCII range");

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

                default:
                    throw std::runtime_error(std::format("Invalid escape sequence '\\{}'", context.cache));
            }
        } else {
            buffer.push_back(context.cache);
        }

        if ('\'' != context.str[++context.range.end])
            throw std::runtime_error("Unterminated character literal");

        context.range.end++;

        buffer.push_back('\'');

        context.token.value = std::move(buffer);
        context.token.identifier = std::hash<std::string_view>{}("Literal");
    }

public:
    Lexer(std::unique_ptr<std::istream> stream, std::shared_ptr<PrefixTree> keywords, std::shared_ptr<PrefixTree> punctuation) : buffer(), cursor{ 1, 1 }, keywords(std::move(keywords)), punctuation(std::move(punctuation)), stream(std::move(stream)) {
        if (!this->stream.get())
            throw std::invalid_argument("Invalid stream pointer");
        std::getline(*this->stream, this->buffer);
    }

    std::pair<Point<std::size_t>, Token> GetNextToken() {
        Context context;
        context.str = this->buffer.c_str();
        context.range.end = this->cursor.x - 1;

        try {
            do {
                while (std::isspace(context.cache = context.str[context.range.end]))
                    context.range.end++;

                context.range.start = context.range.end;
                context.cursor.x = context.range.end + 1;
                context.cursor.y = this->cursor.y;

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
                                context.token.identifier = std::hash<std::string_view>{}("Identifier");
                                context.token.value = std::string(context.str + context.range.start, context.range.end - context.range.start);

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
                            context.token.value = std::string(context.str + context.range.start, context.range.end - context.range.start);
                            context.token.identifier = this->keywords->Search(context.token.value) ? std::hash<std::string_view>{}("Keyword") : std::hash<std::string_view>{}("Identifier");

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

                    context.token.identifier = std::hash<std::string_view>{}("Literal");
                    context.token.value = std::string(context.str + context.range.start, context.range.end - context.range.start);
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
                                if (!ItsOver(context))
                                    continue;

                                context.token.identifier = std::hash<std::string_view>{}("EndOfFile");

                                break;
                            } else if ('*' == context.cache) {
                                context.range.end += 2;

                                do {
                                    do {
                                        context.cache = context.str[context.range.end];

                                        if ('\0' == context.cache) {
                                            if (!ItsOver(context))
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
                            context.token.value = GreedyMatching(context.str + context.range.start, *this->punctuation);
                            context.range.end += context.token.value.length();

                            if (context.range.start != context.range.end) {
                                context.token.identifier = std::hash<std::string_view>{}("Punctuation");
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

                                context.token.identifier = std::hash<std::string_view>{}("Unknown");
                                context.token.value = std::string(context.str + context.range.start, context.range.end - context.range.start);
                            }

                            break;
                    }
                } else if ('\0' == context.cache) {
                    if (!ItsOver(context))
                        continue;

                    context.token.identifier = std::hash<std::string_view>{}("EndOfFile");
                } else {
                    do {
                        context.cache = context.str[++context.range.end];
                    } while (!(std::isspace(context.cache) || '\0' == context.cache));

                    context.token.identifier = std::hash<std::string_view>{}("Unknown");
                    context.token.value = std::string(context.str + context.range.start, context.range.end - context.range.start);
                }

                this->cursor.x = context.range.end + 1;

                return std::make_pair<Point<std::size_t>, Token>(std::move(context.cursor), std::move(context.token));
            } while (1);
        } catch (const std::exception& e) {
            return std::make_pair<Point<std::size_t>, Token>(std::move(context.cursor), Token{ .identifier = std::hash<std::string_view>{}("Error"), .value = e.what() });
        }
    }
};

class AbstractSyntaxTree {
public:
    struct Node {
        std::vector<Node>           children;
        std::string                 name;
        std::optional<std::string>  value;

        void Print(std::uint32_t indentationLevel = 0) const noexcept {
            constexpr static char colors[3] = { '3', '5', '6' };
            std::cout << std::format("\033[3{}m{}\033[0m", colors[(indentationLevel / 2) % 3], this->name);
            if (this->value.has_value())
                std::cout << std::format(" ({})", this->value.value());
            std::cout << '\n';
            indentationLevel += 2;
            std::string prefix(indentationLevel, ' ');
            for (const Node& child : this->children) {
                std::cout << prefix;
                child.Print(indentationLevel);
            }
        }
    };

    Node root;
};

class Parser {
public:
    Parser(Lexer&& lexer) noexcept : lexer(std::move(lexer)) {
        this->Consume();
        this->unaryOperators = { "-", "!", "++", "--", "*", "&" };
        this->binaryOperators = {
            { "=", 0 }, { "*=", 0 }, { "+=", 0 }, { "-=", 0 }, { "/=", 0 }, { "<<=", 0 }, { ">>=", 0 }, { "^=", 0 }, { "|=", 0 }, { "~=", 0 },
            { "+", 1 }, { "-", 1 },
            { "*", 2 }, { "/", 2 }, { "%", 2 },
            { "==", 3 }, { "!=", 3 }, { "<", 3 }, { ">", 3 }, { "<=", 3 }, { ">=", 3 },
            { "&&", 4 }, { "||", 5 }
        };
    }

    AbstractSyntaxTree Parse() {
        AbstractSyntaxTree ast;
        ast.root.name = "Program";

        while (std::hash<std::string_view>{}("EndOfFile") != this->current.second.identifier)
            ast.root.children.push_back(this->ParseStatment());

        return ast;
    }

private:
    using Context = std::pair<Point<std::size_t>, Lexer::Token>;

    Lexer                                               lexer;
    Context                                             current;
    std::queue<Context>                                 future;
    std::unordered_set<std::string_view>                unaryOperators;
    std::unordered_map<std::string_view, std::int32_t>  binaryOperators;

    inline void Consume() {
        if (this->future.empty())
            this->current = this->lexer.GetNextToken();
        else {
            this->current = this->future.front();
            this->future.pop();
        }
    }

    inline void LookAhead() {
        this->future.push(this->lexer.GetNextToken());
    }

    inline bool IsUnaryOperator(std::string_view op) const noexcept {
        return this->unaryOperators.contains(op);
    }

    inline bool IsBinaryOperator(std::string_view op) const noexcept {
        return this->binaryOperators.contains(op);
    }

    inline std::int32_t GetOperatorPrecendece(std::string_view op) const noexcept {
        return this->IsBinaryOperator(op) ? this->binaryOperators.at(op) : -1;
    }

    inline static bool IsBultinType(std::string_view op) noexcept {
        static std::unordered_set<std::string_view> builtinTypes = { "bool", "char", "double", "float", "int", "long", "short", "void" };
        return builtinTypes.contains(op);
    }

    inline static bool IsTypeModifierKeyword(std::string_view op) noexcept {
        static std::unordered_set<std::string_view> builtinTypes = { "const", "constexpr", "mutable", "restrict", "volatile" };
        return builtinTypes.contains(op);
    }

    inline static bool IsTypeModifier(std::string_view op) noexcept {
        static std::unordered_set<std::string_view> builtinTypes = { "*", "&", "&&" };
        return builtinTypes.contains(op) | Parser::IsTypeModifierKeyword(op);
    }

    inline void Expect(std::string_view expected) {
        if (expected != this->current.second.value)
            throw std::runtime_error(std::format("Unexpected token: \'{}\' at ({}:{}:{})", this->current.second.value, "code.cast", this->current.first.y, this->current.first.x));
        
        this->Consume();
    }

    AbstractSyntaxTree::Node ParseAs(std::string&& as) {
        std::string identifier = std::move(this->current.second.value);

        this->Consume();

        return {
            .children = {},
            .name = std::move(as),
            .value = std::move(identifier)
        };
    }

    AbstractSyntaxTree::Node ParseType() {
        AbstractSyntaxTree::Node node = {
            .children = {},
            .name = "Type",
            .value = std::nullopt
        };

        while (Parser::IsTypeModifier(this->current.second.value)) {
            node.children.push_back({
                .children = {},
                .name = "Modifier",
                .value = std::move(this->current.second.value)
            });

            this->Consume();
        }

        if (!(std::hash<std::string_view>{}("Identifier") == this->current.second.identifier || this->IsBultinType(this->current.second.value)))
            throw std::runtime_error("Invalid Type");

        AbstractSyntaxTree::Node type = this->ParseAs("Identifier");

        while (true) {
            if ("::" == this->current.second.value) {
                this->ParseAccessExpression(node);

                continue;
            }

            if ("<" == this->current.second.value) {
                this->ParseTemplate(node);
                
                continue;
            }

            break;
        }

        node.children.push_back(std::move(type));

        while (Parser::IsTypeModifier(this->current.second.value)) {
            node.children.push_back({
                .children = {},
                .name = "Modifier",
                .value = std::move(this->current.second.value)
            });

            this->Consume();
        }

        return node;
    }

    std::vector<AbstractSyntaxTree::Node> ParseBlock() {
        this->Consume();

        std::vector<AbstractSyntaxTree::Node> nodes;

        while ("}" != this->current.second.value)
            nodes.push_back(this->ParseStatment());

        this->Consume();

        return nodes;
    }

    AbstractSyntaxTree::Node ParseBody() {
        return {
            .children = "{" == this->current.second.value ? this->ParseBlock() : (std::vector<AbstractSyntaxTree::Node>){ this->ParseStatment() },
            .name = "Body",
            .value = std::nullopt
        };
    }

    AbstractSyntaxTree::Node ParseInitializerList() {
        this->Consume();

        AbstractSyntaxTree::Node node = {
            .children = {},
            .name = "Initializer List",
            .value = std::nullopt
        };

        if ("}" != this->current.second.value) {
            while (true) {
                if ("." == this->current.second.value) {
                    this->Consume();

                    AbstractSyntaxTree::Node designator = {
                        .children = { this->ParseAs("Identifier"), },
                        .name = "Designator",
                        .value = std::nullopt
                    };

                    this->Expect("=");

                    designator.children.push_back(this->ParseExpression());

                    node.children.push_back(std::move(designator));
                } else if ("[" == this->current.second.value) {
                    this->LookAhead();
                } else node.children.push_back(this->ParseExpression());

                if ("," == this->current.second.value) {
                    this->Consume();
                    
                    continue;
                }

                if ("}" == this->current.second.value)
                    break;

                throw std::runtime_error(std::format("Invalid Initializer List at {}:{}:{}", "code.cast", this->current.first.y, this->current.first.x));
            }
        }

        this->Consume();

        return node;
    }

    void ParseCall(AbstractSyntaxTree::Node& base) {
        this->Consume();

        base = {
            .children = { std::move(base), },
            .name = "Call",
            .value = std::nullopt
        };

        if (")" != this->current.second.value) {
            AbstractSyntaxTree::Node arguments = {
                .children = {},
                .name = "Arguments",
                .value = std::nullopt
            };

            while (true) {
                arguments.children.push_back(this->ParseExpression());

                if ("," == this->current.second.value) {
                    this->Consume();

                    continue;
                }

                if (")" == this->current.second.value)
                    break;

                throw std::runtime_error(std::format("TODO: {}:{}", __FILE__, __LINE__));
            }

            base.children.push_back(std::move(arguments));
        }

        this->Consume();
    }

    void ParseIndexing(AbstractSyntaxTree::Node& base) {
        this->Consume();

        if ("]" == this->current.second.value)
            throw std::runtime_error(std::format("TODO: {}:{}", __FILE__, __LINE__));

        AbstractSyntaxTree::Node arguments = {
            .children = {},
            .name = "Arguments",
            .value = std::nullopt
        };

        AbstractSyntaxTree::Node argument;

        while (true) {
            if (".." == this->current.second.value || "..." == this->current.second.value) {
                arguments.children.push_back({
                    .children = {
                        this->ParseAs("Operator"),
                        this->ParseExpression()
                    },
                    .name = "Range",
                    .value = std::nullopt
                });
            } else {
                argument = this->ParseExpression();

                if (".." == this->current.second.value || "..." == this->current.second.value) {
                    argument = {
                        .children = {
                            std::move(argument),
                            this->ParseAs("Operator")
                        },
                        .name = "Range",
                        .value = std::nullopt
                    };

                    if ("," == this->current.second.value) {
                        this->Consume();

                        arguments.children.push_back(std::move(argument));

                        continue;
                    }

                    if ("]" == this->current.second.value) {
                        arguments.children.push_back(std::move(argument));

                        break;
                    }

                    argument.children.push_back(this->ParseExpression());

                    arguments.children.push_back(std::move(argument));
                } else {
                    arguments.children.push_back({
                        .children = { std::move(argument) },
                        .name = "Expression",
                        .value = std::nullopt
                    });
                }
            }

            if ("," == this->current.second.value) {
                this->Consume();

                continue;
            }

            if ("]" == this->current.second.value)
                break;

            throw std::runtime_error(std::format("TODO: {}:{}", __FILE__, __LINE__));
        }

        this->Consume();

        base = {
            .children = {
                std::move(base),
                std::move(arguments)
            },
            .name = "Indexing",
            .value = std::nullopt
        };
    }

    void ParseTemplate(AbstractSyntaxTree::Node& base) {
        this->Consume();

        AbstractSyntaxTree::Node arguments = {
            .children = {},
            .name = "Arguments",
            .value = std::nullopt
        };

        while (true) {
            if (std::hash<std::string_view>{}("Identifier") == this->current.second.identifier || std::hash<std::string_view>{}("Keyword") == this->current.second.identifier) {
                arguments.children.push_back(this->ParseAs("Identifier"));
            } else if (std::hash<std::string_view>{}("Literal") == this->current.second.identifier) {
                arguments.children.push_back(this->ParseAs("Literal"));
            } else {
                arguments.children.push_back(this->ParseExpression());
            }

            if ("," == this->current.second.value) {
                this->Consume();
                continue;
            }

            if (">" == this->current.second.value) {
                this->Consume();
                break;
            }

            throw std::runtime_error("Unexpected token in template arguments: " + this->current.second.value);
        }

        base = {
            .children = {
                std::move(base),
                std::move(arguments)
            },
            .name = "Template",
            .value = std::nullopt
        };
    }

    void ParseAccessExpression(AbstractSyntaxTree::Node& base) {
        base = {
            .children = {
                std::move(base),
                this->ParseAs("Operator"),
                this->ParseAs("Identifier")
            },
            .name = "Access Expression",
            .value = std::nullopt
        };
    }

    void ParseChain(AbstractSyntaxTree::Node& base) {
        while (true) {
            if ("." == this->current.second.value || "->" == this->current.second.value || "::" == this->current.second.value) {
                this->ParseAccessExpression(base);

                continue;
            }

            if ("(" == this->current.second.value) {
                this->ParseCall(base);

                continue;
            }

            if ("[" == this->current.second.value) {
                this->ParseIndexing(base);

                continue;
            }

            if ("<" == this->current.second.value) {
                this->ParseTemplate(base);

                if ("::" == this->current.second.value) {
                    this->ParseAccessExpression(base);

                    continue;
                }

                if ("(" == this->current.second.value) {
                    this->ParseCall(base);

                    continue;
                }

                if ("." == this->current.second.value || "->" == this->current.second.value || "[" == this->current.second.value) {
                    this->ParseIndexing(base);

                    throw std::runtime_error(std::format("TODO: {}:{}", __FILE__, __LINE__));
                }
            }

            break;
        }
    }

    AbstractSyntaxTree::Node ParseParameter() {
        AbstractSyntaxTree::Node node = {
            .children = {
                this->ParseType(),
                this->ParseAs("Identifier")
            },
            .name = "Parameter",
            .value = std::nullopt
        };

        if ("=" == this->current.second.value) {
            this->Consume();

            node.children.push_back(this->ParseExpression());
        }

        return node;
    }

    AbstractSyntaxTree::Node ParseParameters() {
        AbstractSyntaxTree::Node node = {
            .children = {},
            .name = "Parameters",
            .value = std::nullopt
        };

        while (true) {
            node.children.push_back(this->ParseParameter());

            if ("," == this->current.second.value) {
                this->Consume();

                continue;
            }

            if (")" == this->current.second.value)
                break;

            throw std::runtime_error(std::format("TODO: {}:{}", __FILE__, __LINE__));
        }

        return node;
    }

    AbstractSyntaxTree::Node ParseLambdaExpression() {
        this->Consume();

        AbstractSyntaxTree::Node node = {
            .children = {},
            .name = "Lambda Expression",
            .value = std::nullopt
        };

        if ("]" != this->current.second.value) {
            AbstractSyntaxTree::Node foo = {
                .children = {},
                .name = "Capture",
                .value = std::nullopt
            };

            while (true) {
                foo.children.push_back({
                    .children = { this->ParseAs("Identifier") },
                    .name = "Capture", // TODO melhorar isso
                    .value = std::nullopt
                });

                if ("," == this->current.second.value) {
                    this->Consume();

                    continue;
                }

                if ("]" == this->current.second.value)
                    break;
            }

            node.children.push_back(foo);
        }

        this->Expect("]");

        this->Expect("(");

        if (")" != this->current.second.value) {
            node.children.push_back(this->ParseParameters());
        }

        this->Consume();

        if ("->" == this->current.second.value) {
            this->Consume();

            node.children.push_back({
                .children = { this->ParseType() },
                .name = "Return Type",
                .value = std::nullopt
            });
        }

        this->Expect("=>");

        node.children.push_back(this->ParseBody());

        return node;
    }

    AbstractSyntaxTree::Node ParseTypeCast() {
        this->Consume();

        AbstractSyntaxTree::Node node = {
            .children = { this->ParseType() },
            .name = "Type Cast",
            .value = std::nullopt
        };

        this->Expect(")");

        node.children.push_back(this->ParseExpression());

        return node;
    }

    AbstractSyntaxTree::Node ParseOperand() {
        AbstractSyntaxTree::Node node;

        if (std::hash<std::string_view>{}("Keyword") == this->current.second.identifier) {
            if ("false" == this->current.second.value || "true" == this->current.second.value)
                return this->ParseAs("Literal");
            else throw std::runtime_error("Unexpected token in operand. " + this->current.second.value);
        } else if (std::hash<std::string_view>{}("Literal") == this->current.second.identifier) {
            this->LookAhead();

            if (this->IsUnaryOperator(this->future.back().second.value))
                return this->ParseUnaryExpression();

            return this->ParseAs("Literal");
        } else if (std::hash<std::string_view>{}("Identifier") == this->current.second.identifier) {
            this->LookAhead();

            if (this->IsUnaryOperator(this->future.back().second.value))
                return this->ParseUnaryExpression();

            node = this->ParseAs("Identifier");
        } else if ("(" == this->current.second.value) {
            assert(this->future.empty());

            this->LookAhead();

            if (Parser::IsTypeModifierKeyword(this->future.back().second.value))
                return this->ParseTypeCast();

            if (Parser::IsBultinType(this->future.back().second.value)) {
                this->LookAhead();

                if ("(" != this->future.back().second.value) {
                    this->Consume();

                    node = this->ParseOperand();

                    this->Expect(")");

                    return node;
                }

                return this->ParseTypeCast();
            }

            while (")" != this->future.back().second.value)
                this->LookAhead();

            this->LookAhead();

            if (std::hash<std::string_view>{}("Literal") == this->future.back().second.identifier)
                return this->ParseTypeCast();

            this->Consume();

            node = this->ParseExpression();

            this->Expect(")");
        } else if (this->IsUnaryOperator(this->current.second.value))
            return this->ParseUnaryExpression();
        else if ("[" == this->current.second.value) {
            node = this->ParseLambdaExpression();

            if ("(" != this->current.second.value)
                return node;

            this->ParseCall(node);
        } else if ("{" == this->current.second.value)
            node = this->ParseInitializerList();
        else throw std::runtime_error("Unexpected token in operand. " + this->current.second.value);

        this->ParseChain(node);

        return node;
    }

    AbstractSyntaxTree::Node ParseUnaryExpression() {
        return {
            .children = this->IsUnaryOperator(this->current.second.value) ? (std::vector<AbstractSyntaxTree::Node>){ this->ParseAs("Operator"), this->ParseOperand() } : (std::vector<AbstractSyntaxTree::Node>){ this->ParseOperand(), this->ParseAs("Operator") },
            .name = "Unary Expression",
            .value = std::nullopt
        };
    }

    AbstractSyntaxTree::Node ParseBinaryExpression(std::int32_t minPrecedence) {
        AbstractSyntaxTree::Node left = this->ParseOperand();

        while (true) {
            std::int32_t opPrecendece = this->GetOperatorPrecendece(this->current.second.value);

            if (opPrecendece < minPrecedence)
                break;

            left = {
                .children = {
                    std::move(left),
                    this->ParseAs("Operator"),
                    this->ParseBinaryExpression(opPrecendece + 1)
                },
                .name = "Binary Expression",
                .value = std::nullopt
            };
        }

        return left;
    }

    void ParseTernayExpression(AbstractSyntaxTree::Node& base) {
        this->Consume();

        AbstractSyntaxTree::Node trueCase = this->ParseExpression();

        this->Expect(":");

        base = {
            .children = {
                std::move(base),
                std::move(trueCase),
                this->ParseExpression()
            },
            .name = "Ternary Expression",
            .value = std::nullopt
        };
    }

    AbstractSyntaxTree::Node ParseExpression() {
        AbstractSyntaxTree::Node node = this->ParseBinaryExpression(0);

        if ("?" == this->current.second.value)
            this->ParseTernayExpression(node);

        return node;
    }

    std::vector<AbstractSyntaxTree::Node> ParseInstances() {
        std::vector<AbstractSyntaxTree::Node> instances;

        AbstractSyntaxTree::Node instance;

        while (true) {
            instance = {
                .children = { this->ParseAs("Identifier") },
                .name = "Instance",
                .value = std::nullopt
            };

            if ("=" == this->current.second.value) {
                this->Consume();

                instance.children.push_back(this->ParseExpression());
            }

            instances.push_back(std::move(instance));

            if ("," == this->current.second.value) {
                this->Consume();

                continue;
            }

            break;
        }

        return instances;
    }

    AbstractSyntaxTree::Node ParseVariableDeclaration() {
        AbstractSyntaxTree::Node node = {
            .children = { this->ParseType(), },
            .name = "Variable Declaration",
            .value = std::nullopt
        };

        std::vector instance = this->ParseInstances();

        for (auto& instanceNode : instance)
            node.children.push_back(std::move(instanceNode));

        return node;
    }

    AbstractSyntaxTree::Node ParseStructure() {
        AbstractSyntaxTree::Node node = this->ParseAs("Structure");

        if (std::hash<std::string_view>{}("Identifier") == this->current.second.identifier)
            node.children.push_back(this->ParseAs("Identifier"));

        if (";" != this->current.second.value) {
            if (":" == this->current.second.value) {
                throw std::runtime_error("Not Implemented");
            }

            if ("{" != this->current.second.value)
                throw std::runtime_error(std::format("TODO: {}:{}", __FILE__, __LINE__));

            node.children.push_back({
                .children = this->ParseBlock(),
                .name = "Block",
                .value = std::nullopt
            });

            if (std::hash<std::string_view>{}("Identifier") == this->current.second.identifier) {
                std::vector instance = this->ParseInstances();

                for (auto& instanceNode : instance)
                    node.children.push_back(std::move(instanceNode));
            }
        }

        return node;
    }

    AbstractSyntaxTree::Node ParseStatment() {
        AbstractSyntaxTree::Node node;

        if ("{" == this->current.second.value)
            return {
                .children = this->ParseBlock(),
                .name = "Block",
                .value = std::nullopt
            };

        if ("struct" == this->current.second.value || "class" == this->current.second.value || "union" == this->current.second.value)
            node = ParseStructure();
        else
            node = this->ParseVariableDeclaration();

        this->Expect(";");

        return node;
    }
};

int main(int argc, const char** argv) {
    std::shared_ptr keywords = std::make_shared<PrefixTree>();

    keywords->Insert({ "abstract", "auto", "bool", "break", "case", "catch", "char", "class", "concept", "const", "constexpr", "continue", "decltype", "do", "double", "else", "enum", "export", "extern", "false", "for", "friend", "global", "if", "import", "int", "interface", "long", "module", "mutable", "namespace", "noexcept", "operator", "override", "private", "protected" "public", "reinterpret_cast", "restrict", "return", "short", "static", "static_assert", "static_cast", "struct", "switch", "template", "this", "throw", "true", "typedef", "typename", "using", "virtual", "volatile", "void", "while" });

    std::shared_ptr punctuation = std::make_shared<PrefixTree>();

    punctuation->Insert({ "!", "#", "$", "%", "&", "(", ")", "*", "+", ",", "-", ".", "/", ":", ";", "<", "=", ">", "?", "@", "[", "]", "^", "{", "|", "}", "~" });
    punctuation->Insert({ "&&", "++", "--", "..", "...", "::", "<<", "==", ">>", "[[", "||", "]]" });
    punctuation->Insert({ "!=", "*=", "+=", "-=", "/=", "<<=", ">>=", "^=", "|=", "~=", "->", "=>" });

    Parser parser(Lexer(std::make_unique<std::ifstream>(argv[1]), std::move(keywords), std::move(punctuation)));

    std::setlocale(LC_ALL, "en_US.UTF-8");

    AbstractSyntaxTree ast = parser.Parse();
    ast.root.Print();

    return 0;
}

/*
TODO:
    namespaces
    classes, structs, interfaces & concepts.
    templates.
    ``` c
        func()();
    access structs components.
    lambda expressions.
    [a variable type for code thats can be executed](maybe)
    ternay operator
    solve this 1..2
    fazer um jogo sobre troca de corpo
    type cast
*/ 