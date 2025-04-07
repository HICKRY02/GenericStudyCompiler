#include <array>
#include <bitset>
#include <cassert>
#include <cctype>
#include <cstddef>
#include <cstdint>
#include <cstring>
#include <deque>
#include <exception>
#include <filesystem>
#include <format>
#include <fstream>
#include <functional>
#include <iostream>
#include <iomanip>
#include <list>
#include <locale>
#include <map>
#include <memory>
#include <set>
#include <sstream>
#include <stack>
#include <stdexcept>
#include <string>
#include <string_view>
#include <unordered_map>
#include <unordered_set>
#include <vector>

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
        enum class Type : std::uint8_t {
            EndOfFile,
            Punctuation,
            Identifier,
            Literal,
            Keyword,
            Unknown,
            Error
        };

        struct Metadata {
            Type                        type;
            Range<Point<std::size_t>>   range;
        };

        std::string data;
        Metadata    metadata;
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

    bool LoadNextLine(Context& context) {
        if (std::getline(*this->stream, this->buffer)) {
            context.str = this->buffer.c_str();
            context.range.end = 0;
            this->cursor.y++;
            return true;
        }

        return false;
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

    void HandleCharacterLiteral(Context& context) {
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

public:
    Lexer(std::unique_ptr<std::istream>&& stream, std::shared_ptr<PrefixTree>&& keywords, std::shared_ptr<PrefixTree>&& punctuation) : buffer(), cursor{ 1, 1 }, keywords(std::move(keywords)), punctuation(std::move(punctuation)), stream(std::move(stream)) {
        if (!this->stream.get())
            throw std::invalid_argument("Invalid stream pointer"); // TODO: No such file or directory
        std::getline(*this->stream, this->buffer);
    }

    Token GetNextToken() {
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
};

template<typename T>
class IdentifierGenerator {
    T   accumulator;

public:
    IdentifierGenerator() : accumulator() {}

    T operator()() {
        return accumulator++;
    }
};

/* Grammar:

Punctuation -> "!" | "#" | "$" | "%" | "&" | "(" | ")" | "*" | "+" | "|" | "-" | "." | "/" | ":" | ";" | "<" | "=" | ">" | "?" | "@" | "[" | "]" | "^" | "{" | "|" | "}" | "~" "&&" | "++" | "--" | ".." | "..." | "::" | "<<" | "==" | ">>" | "[[" | "||" | "]]" "!=" | "*=" | "+=" | "-=" | "/=" | "<<=" | ">>=" | "^=" | "|=" | "~=" | "->" | "=>"

𝑆'  -> $

𝐺 = (𝑁, Σ, 𝑃, 𝑆)

*/

class Parser {
public:
    using SizeType      = std::int32_t;
    using State         = std::int32_t;
    using Symbol        = std::string;

    struct Tree {
        struct Node {
            std::vector<Node>   children;
            std::string         label;

            void Print(std::uint32_t indentationLevel = 0) const noexcept {
                constexpr static char colors[3] = { '3', '5', '6' };
                std::cout << std::format("\033[3{}m{}\033[0m", colors[(indentationLevel / 2) % 3], this->label);
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

    struct Action {
        enum class Type : std::uint8_t {
            Error,
            Shift,
            Reduce,
            Accept
        };

        Type    type;
        State   value;
    };

    struct Production {
        Symbol              lhs;
        std::vector<Symbol> rhs;
    };

    struct Model {
        using ActionTable           = std::map<std::pair<State, Symbol>, Action>;
        using ProductionTable       = std::vector<Production>;
        using GotoTable             = std::map<std::pair<State, Symbol>, State>;
        using TokenClassifier       = std::function<Symbol(Lexer::Token&)>;
        using TokenNodifier         = std::function<Tree::Node(Lexer::Token&)>;

        ActionTable             actionTable;
        ProductionTable         productions;
        GotoTable               gotoTable;
        TokenClassifier         classifier;
        TokenNodifier           tokenNodifier;

        void SetAction(State state, std::string symbol, Action action) {
            this->actionTable[{ state, symbol }] = action;
        }

        void RemoveAction(State state, std::string symbol) {
            this->actionTable.erase({ state, symbol });
        }

        bool HasAction(State state, std::string symbol) const {
            return this->actionTable.end() == this->actionTable.find({ state, symbol });
        }

        Action GetAction(State state, std::string symbol) const {
            auto it = this->actionTable.find({ state, symbol });
            if (this->actionTable.end() == it)
                throw std::runtime_error(std::format("Action ({}:\"{}\") not found", state, symbol));
            return it->second;
        }

        void SetGoto(State state, std::string symbol, State target) {
            this->gotoTable[{ state, symbol }] = target;
        }

        void RemoveGoto(State state, std::string symbol) {
            this->gotoTable.erase({ state, symbol });
        }

        bool HasGoto(State state, std::string symbol) const {
            return this->gotoTable.end() == this->gotoTable.find({ state, symbol });
        }

        State GetGoto(State state, std::string symbol) const {
            auto it = this->gotoTable.find({ state, symbol });
            if (this->gotoTable.end() == it)
                throw std::runtime_error(std::format("Goto ({}:\"{}\") not found", state, symbol));
            return it->second;
        }
    };

    class Grammar {
        struct Item {
            Symbol              lhs;
            std::vector<Symbol> rhs;
            SizeType            dot;
            Symbol              lookahead;

            bool operator<(const Item& other) const {
                if (this->lhs != other.lhs)
                    return this->lhs < other.lhs;
                if (this->rhs != other.rhs)
                    return this->rhs < other.rhs;
                if (this->dot != other.dot)
                    return this->dot < other.dot;
                return this->lookahead < other.lookahead;
            }

            bool operator==(const Item& other) const {
                return this->lhs == other.lhs && this->rhs == other.rhs && this->dot == other.dot && this->lookahead == other.lookahead;
            }
        };

        std::shared_ptr<Model>                      model;
        std::set<Symbol>                            terminals;
        std::set<Symbol>                            nonTerminals;
        Symbol                                      𝑆;
        std::map<Symbol, std::set<Symbol>>          firstSets;
        std::vector<std::set<Item>>                 C;

        void ComputeFirstSets() {
            for (auto& t : terminals)
                firstSets[t] = {t};
            for (auto& nt : nonTerminals)
                firstSets[nt] = {};
            bool changed;
            do {
                changed = false;
                for (auto& p : this->model->productions) {
                    auto& A = p.lhs;
                    auto& alpha = p.rhs;
                    auto& firstA = firstSets[A];
                    std::set<Symbol> temp;
                    bool allEpsilon = true;
                    for (auto& X : alpha) {
                        for (auto& sym : firstSets[X])
                            if (sym != "ε")
                                temp.insert(sym);
                        if (firstSets[X].count("ε") == 0) {
                            allEpsilon = false;
                            break;
                        }
                    }
                    if (allEpsilon) temp.insert("ε");
                    size_t before = firstA.size();
                    firstA.insert(temp.begin(), temp.end());
                    if (firstA.size() != before) changed = true;
                }
            } while (changed);
        }
    
        std::set<Symbol> First(const std::vector<Symbol>& symbols) const {
            std::set<Symbol> result;
            bool allEpsilon = true;
            for (auto& X : symbols) {
                for (auto& sym : firstSets.at(X))
                    if (sym != "ε")
                        result.insert(sym);
                if (firstSets.at(X).count("ε") == 0) {
                    allEpsilon = false;
                    break;
                }
            }
            if (allEpsilon) result.insert("ε");
            return result;
        }
    
        // Fecho (closure) de itens
        std::set<Item> Closure(const std::set<Item>& I) const {
            std::set<Item> closureSet = I;
            bool added;
            do {
                added = false;
                for (auto& it : closureSet) {
                    if (it.dot < it.rhs.size()) {
                        Symbol B = it.rhs[it.dot];
                        if (nonTerminals.count(B)) {
                            std::vector<Symbol> beta(it.rhs.begin() + it.dot + 1, it.rhs.end());
                            beta.push_back(it.lookahead);
                            auto lookaheadSet = this->First(beta);
                            for (auto& p : this->model->productions) {
                                if (p.lhs == B) {
                                    for (auto& la : lookaheadSet) {
                                        Item newItem = { B, p.rhs, 0, la };
                                        if (!closureSet.count(newItem)) {
                                            closureSet.insert(newItem);
                                            added = true;
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            } while (added);
            return closureSet;
        }
    
        // Função GOTO
        std::set<Item> GotoFunction(const std::set<Item>& I, const Symbol& X) const {
            std::set<Item> J;
            for (auto& it : I)
                if (it.dot < it.rhs.size() && it.rhs[it.dot] == X)
                    J.insert({ it.lhs, it.rhs, it.dot + 1, it.lookahead });
            return this->Closure(J);
        }
    
        // Construção da coleção canônica de itens
        void Items() {
                    // Gramática aumentada S' -> startSymbol
            Symbol augmentedStart = this->𝑆 + "'";
            // Remove produção aumentada anterior, se existir
            this->model->productions.erase(std::remove_if(this->model->productions.begin(), this->model->productions.end(),
                [&](const Production &p) { return p.lhs == augmentedStart; }), this->model->productions.end());
            this->AddNonTerminal(augmentedStart);
            this->model->productions.insert(this->model->productions.begin(), { augmentedStart, { this->𝑆 } });
            this->ComputeFirstSets();
            Item startItem = { augmentedStart, { this->𝑆 }, 0, "EndOfFile" };
            this->C.clear();
            this->C.push_back(this->Closure({startItem}));
            bool added;
            do {
                added = false;
                std::set<Symbol> symbols = terminals;
                symbols.insert(nonTerminals.begin(), nonTerminals.end());
                for (int i = 0; i < (int)this->C.size(); ++i) {
                    for (auto& X : symbols) {
                        auto gotoSet = this->GotoFunction(C[i], X);
                        if (!gotoSet.empty() && std::find(this->C.begin(), this->C.end(), gotoSet) == this->C.end()) {
                            this->C.push_back(gotoSet);
                            added = true;
                        }
                    }
                }
            } while (added);
        }

        int FindState(const std::set<Item>& I) const {
            for (int i = 0; i < (int)this->C.size(); ++i)
                if (this->C[i] == I)
                    return i;
            return -1;
        }
    
        int FindProductionIndex(const Symbol& lhs, const std::vector<Symbol>& rhs) const {
            for (int i = 0; i < (int)this->model->productions.size(); ++i)
                if (this->model->productions[i].lhs == lhs && this->model->productions[i].rhs == rhs)
                    return i;
            return -1;
        }

    public:
        std::shared_ptr<Model> GetModel() noexcept { return this->model; }

        Grammar(const std::string& 𝑆) : model(std::make_shared<Model>()), 𝑆(𝑆) {
            this->AddTerminal("EndOfFile");
            this->AddTerminal("Identifier");
            this->AddTerminal("Literal");

            this->model->classifier = [this](Lexer::Token& token) -> Symbol {
                switch (token.metadata.type) {
                    case Lexer::Token::Type::EndOfFile:
                        return "EndOfFile";

                    case Lexer::Token::Type::Error:
                        throw std::runtime_error(token.data);

                    case Lexer::Token::Type::Identifier:
                        return "Identifier";

                    case Lexer::Token::Type::Keyword:
                        return token.data;

                    case Lexer::Token::Type::Literal:
                        return "Literal";

                    case Lexer::Token::Type::Punctuation:
                        return token.data;

                    case Lexer::Token::Type::Unknown:
                        throw std::runtime_error("Unknown");

                    default:
                        throw std::bad_exception();
                }
            };

            this->model->tokenNodifier = [](Lexer::Token& token) -> Tree::Node {
                Tree::Node node;

                switch (token.metadata.type) {
                    case Lexer::Token::Type::EndOfFile:
                        node.label = "EndOfFile";
                        break;

                    case Lexer::Token::Type::Error:
                        throw std::runtime_error(token.data);

                    case Lexer::Token::Type::Identifier:
                        node.label = std::format("Identifier: \"{}\"", token.data);
                        break;

                    case Lexer::Token::Type::Keyword:
                        node.label = token.data;
                        break;

                    case Lexer::Token::Type::Literal:
                        node.label = std::format("Literal: \"{}\"", token.data);
                        break;

                    case Lexer::Token::Type::Punctuation:
                        node.label = token.data;
                        break;

                    case Lexer::Token::Type::Unknown:
                        throw std::runtime_error("Unknown");

                    default:
                        throw std::bad_exception();
                }

                return node;
            };
        }

        void AddTerminal(std::string t) {
            this->terminals.insert(t);
        }

        void AddNonTerminal(std::string nt) {
            this->nonTerminals.insert(nt);
        }

        void AddProduction(std::string&& lhs, std::vector<std::string>&& rhs) {
            this->model->productions.push_back({ std::move(lhs), std::move(rhs) });
        }

        void BuildParsingTable() {
            this->model->actionTable.clear();
            this->model->gotoTable.clear();
            this->Items();
            for (int i = 0; i < (int)this->C.size(); ++i) {
                for (auto& it : this->C[i]) {
                    if (it.dot < it.rhs.size()) {
                        Symbol a = it.rhs[it.dot];
                        if (terminals.count(a))
                            this->model->SetAction(i, a, { Action::Type::Shift, this->FindState(this->GotoFunction(this->C[i], a)) });
                    } else {
                        if (it.lhs == this->𝑆 + "'")
                            this->model->SetAction(i, "EndOfFile", { Action::Type::Accept, 0 });
                        else
                            this->model->SetAction(i, it.lookahead, { Action::Type::Reduce, this->FindProductionIndex(it.lhs, it.rhs) });
                    }
                }
                for (auto& A : nonTerminals) {
                    auto gotoSet = this->GotoFunction(C[i], A);
                    if (!gotoSet.empty()) {
                        this->model->SetGoto(i, A, this->FindState(gotoSet));
                    }
                }
            }
        }
    };

private:
    Lexer                   lexer;
    std::shared_ptr<Model>  model;

public:
    Parser(Lexer&& lexer, std::shared_ptr<Model>&& model) : lexer(std::move(lexer)), model(std::move(model)) {}

    Tree Parse() {
        std::stack<Tree::Node> nodeStack;
        std::stack<int> stateStack;

        stateStack.push(0);

        Tree::Node bufferNode;
        Lexer::Token currentToken = this->lexer.GetNextToken();
        State currentState;
        Symbol currentSym;
        Action act;
        Production p;

        while (true) {
            currentState = stateStack.top();

            currentSym = this->model->classifier(currentToken);

            act = this->model->GetAction(currentState, currentSym);

            switch (act.type) {
                case Action::Type::Shift:
                    stateStack.push(act.value);
                    nodeStack.push(this->model->tokenNodifier(currentToken));
                    currentToken = this->lexer.GetNextToken();

                    break;

                case Action::Type::Reduce:
                    p = this->model->productions[act.value];

                    bufferNode.label = p.lhs;

                    for (SizeType i = 0; i < (SizeType)p.rhs.size(); i++) {
                        bufferNode.children.push_back(std::move(nodeStack.top()));
                        nodeStack.pop();

                        stateStack.pop();
                    }

                    std::reverse(bufferNode.children.begin(), bufferNode.children.end());

                    stateStack.push(this->model->GetGoto(stateStack.top(), p.lhs));
                    nodeStack.push(std::move(bufferNode));

                    break;

                case Action::Type::Accept:
                    return { .root = std::move(nodeStack.top()) };

                default:
                    throw std::runtime_error("Parsing error");
            }
        }
    }
};

inline std::string trim(const std::string& s) {
    auto front = std::find_if_not(s.begin(), s.end(), [](int c){ return std::isspace(c); });
    auto back  = std::find_if_not(s.rbegin(), s.rend(), [](int c){ return std::isspace(c); }).base();
    if (front >= back) return "";
    return std::string(front, back);
}

void LoadGrammarFromString(std::istringstream iss, Parser::Grammar& grammar) {
    std::string buffer;
    std::size_t lineNum = 0;
    std::size_t pos;
    std::string lhs;
    std::istringstream rhsIss;
    std::vector<std::string> rhs;

    while (std::getline(iss, buffer)) {
        ++lineNum;
        buffer = trim(buffer);

        if (buffer.empty()) 
            continue;

        if ('#' == buffer[0])
            continue;

        if (std::string::npos == (pos = buffer.find("->")))
            throw std::invalid_argument(std::format("Error on line {}: Invalid format: Missing \"->\"", lineNum));

        lhs = trim(buffer.substr(0, pos));
        rhsIss.clear();
        rhsIss.str(trim(buffer.substr(pos + 2)));

        if (lhs.empty())
            throw std::invalid_argument(std::format("Error on line {}: Empty LHS", lineNum));

        grammar.AddNonTerminal(lhs);
    
        rhs.clear();
        while (rhsIss >> buffer)
            rhs.push_back(std::move(buffer));

        if (rhs.empty())
            throw std::invalid_argument(std::format("Error on line {}: Empty RHS", lineNum));

        if ("ε" == rhs[0]) {
            if (1 != rhs.size())
                throw std::runtime_error("DOES NOT KNOW WHAT TO DO!");

            rhs.clear();
        }

        std::cout << "Adding new production: " << lhs << " { ";
        for (const auto& rhsIt : rhs)
            std::cout << rhsIt << " ";
        std::cout << '}' << std::endl;

        grammar.AddProduction(std::move(lhs), std::move(rhs));
    }

    grammar.BuildParsingTable();
}

void LoadGrammarFromFile(const std::string& filename, Parser::Grammar& grammar) {
    std::ifstream file(filename, std::ios::binary);
    if (!file)
        throw std::runtime_error("Unable to open: " + filename);
    std::ostringstream buf;
    buf << file.rdbuf();
    LoadGrammarFromString(std::istringstream(buf.str()), grammar);
}

int main(int argc, const char** argv) {
    std::shared_ptr keywords = std::make_shared<PrefixTree>();

    std::initializer_list<std::string_view> terminalKeywords = {
        "auto", "bool", "case", "catch", "char", "const", "constexpr", "continue", "default", "do", "double", "else", "false", "float", "for", "if", "int", "long", "mutable", "namespace", "private", "protected", "return", "short", "switch", "throw", "true", "void", "volatile", "while"
    };

    keywords->Insert(terminalKeywords);

    std::shared_ptr punctuation = std::make_shared<PrefixTree>();

    std::initializer_list<std::string_view> terminalPunctuaction = {
        "!", "#", "$", "%", "&", "(", ")", "*", "+", ",", "-", ".", "/", ":", ";", "<", "=", ">", "?", "@", "[", "]", "^", "{", "|", "}", "~",
        "&&", "++", "--", "..", "...", "::", "<<", "==", ">>", "[[", "||", "]]",
        "!=", "%=", "&=", "*=", "+=", "-=", "/=", "<=", "<<=", ">=", ">>=", "^=", "|=", "~=", "->", "=>"
    };

    punctuation->Insert(terminalPunctuaction);

    Parser::Grammar grammar("S");

    for (const auto& terminal : terminalKeywords)
        grammar.AddTerminal(std::string(terminal));

    for (const auto& terminal : terminalPunctuaction)
        grammar.AddTerminal(std::string(terminal));

    LoadGrammarFromFile("grammar.txt", grammar);

    Parser parser(Lexer(std::make_unique<std::ifstream>(argv[1]), std::move(keywords), std::move(punctuation)), grammar.GetModel());

    parser.Parse().root.Print();

    return 0;
}

// static memory allocation

/*

auto file = std::make_unique<std::ifstream>(argv[1]);

auto lexer = Lexer::Create(std::move(file));

Lexer::Token token;

std::unordered_map<Lexer::Token::Type, std::string_view> typeTable = {
    { Lexer::Token::Type::EndOfFile, "EndOfFile" },
    { Lexer::Token::Type::Error, "Error" },
    { Lexer::Token::Type::Identifier, "Identifier" },
    { Lexer::Token::Type::Keyword, "Keyword" },
    { Lexer::Token::Type::Literal, "Literal" },
    { Lexer::Token::Type::Punctuation, "Punctuation" },
    { Lexer::Token::Type::Unknown, "Unknown" }
};

while (Lexer::Token::Type::EndOfFile != (token = lexer.GetNextToken()).metadata.type) {
    std::cout << std::format(
        "{{\n\t\033[33mdata\033[0m: \"\033[32m{}\033[0m\",\n\t\033[33mmetadata\033[0m: {{\n\t\t\033[35mtype\033[0m: \"\033[32m{}\033[0m\",\n\t\t\033[35mrange\033[0m: {{\n\t\t\t\033[36mstart\033[0m: {{\n\t\t\t\t\033[33mx\033[0m: \033[32m{}\033[0m,\n\t\t\t\t\033[33my\033[0m: \033[32m{}\033[0m\n\t\t\t}}, \033[36mend\033[0m: {{\n\t\t\t\t\033[33mx\033[0m: \033[32m{}\033[0m,\n\t\t\t\t\033[33my\033[0m: \033[32m{}\033[0m\n\t\t\t}}\n\t\t}}\n\t}}\n}}\033[0m"
        " "
        "{}:{}:{} {}:{}:{}",
        token.data,
        typeTable[token.metadata.type],
        token.metadata.range.start.x, token.metadata.range.start.y,
        token.metadata.range.end.x, token.metadata.range.end.y,
        argv[1], token.metadata.range.start.y, token.metadata.range.start.x,
        argv[1], token.metadata.range.end.y, token.metadata.range.end.x
    ) << std::endl;
}

*/