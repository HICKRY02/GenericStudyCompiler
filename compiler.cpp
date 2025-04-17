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
#include <queue>
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
    std::shared_ptr<PrefixTree> GetKeywords() noexcept {
        return this->keywords;
    }

    std::shared_ptr<PrefixTree> GetPunctuation() noexcept {
        return this->punctuation;
    }

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
class Serializer {
public:
    static void Serialize(const T& obj, std::ostream& os) {
        static_assert(sizeof(T) == 0, "Serialize not implemented for this type.");
    }

    static T Deserialize(std::istream& is) {
        static_assert(sizeof(T) == 0, "Deserialize not implemented for this type.");
    }
};

template<typename T>
concept Serializable = requires(T& obj, std::istream& is, std::ostream& os) {
    { Serializer<T>::Serialize(obj, os) };
    { Serializer<T>::Deserialize(is) } -> std::same_as<T>;
};

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
        using AttributeTable        = std::unordered_map<Symbol, std::unordered_set<std::string>>;

        ActionTable      actionTable;
        ProductionTable  productions;
        GotoTable        gotoTable;
        TokenClassifier  classifier;
        TokenNodifier    tokenNodifier;
        AttributeTable   attributesTable;

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

        bool NonTerminalHasAttribute(std::string nonTerminal, std::string attribute) const noexcept {
            if (this->attributesTable.contains(nonTerminal))
                return this->attributesTable.at(nonTerminal).contains(attribute);
            return false;
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

        std::shared_ptr<Model>                                              model;
        std::set<Symbol>                                                    terminals;
        std::unordered_map<Symbol, std::unordered_set<std::string>>         attributesTable;
        std::set<Symbol>                                                    nonTerminals;
        std::unordered_map<Symbol, std::vector<std::vector<std::string>>>   productions;
        Symbol                                                              𝑆;
        std::map<Symbol, std::set<Symbol>>                                  firstSets;
        std::vector<std::set<Item>>                                         C;

        friend class Serializer<Parser::Grammar>;

        bool NonTerminalHasAttribute(std::string nonTerminal, std::string attribute) const noexcept {
            if (this->attributesTable.contains(nonTerminal))
                return this->attributesTable.at(nonTerminal).contains(attribute);
            return false;
        }

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
                    if (allEpsilon)
                        temp.insert("ε");
                    std::size_t before = firstA.size();
                    firstA.insert(temp.begin(), temp.end());
                    if (firstA.size() != before)
                        changed = true;
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
    
        std::set<Item> Closure(const std::set<Item>& I) const {
            std::set<Item> closureSet = I;
            bool added;
            do {
                added = false;
                for (auto& it : closureSet)
                    if (it.dot < it.rhs.size()) {
                        Symbol B = it.rhs[it.dot];
                        if (nonTerminals.count(B)) {
                            std::vector<Symbol> beta(it.rhs.begin() + it.dot + 1, it.rhs.end());
                            beta.push_back(it.lookahead);
                            auto lookaheadSet = this->First(beta);
                            for (auto& p : this->model->productions)
                                if (p.lhs == B)
                                    for (auto& la : lookaheadSet) {
                                        Item newItem = { B, p.rhs, 0, la };
                                        if (!closureSet.count(newItem)) {
                                            closureSet.insert(newItem);
                                            added = true;
                                        }
                                    }
                        }
                    }
            } while (added);
            return closureSet;
        }
    
        std::set<Item> GotoFunction(const std::set<Item>& I, const Symbol& X) const {
            std::set<Item> J;
            for (auto& it : I)
                if (it.dot < it.rhs.size() && it.rhs[it.dot] == X)
                    J.insert({ it.lhs, it.rhs, it.dot + 1, it.lookahead });
            return this->Closure(J);
        }
    
        void Items() {
            Symbol augmentedStart = this->𝑆 + "'";
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
                for (int i = 0; i < (int)this->C.size(); ++i)
                    for (auto& X : symbols) {
                        auto gotoSet = this->GotoFunction(C[i], X);
                        if (!gotoSet.empty() && std::find(this->C.begin(), this->C.end(), gotoSet) == this->C.end()) {
                            this->C.push_back(gotoSet);
                            added = true;
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

        void AddAttributes(std::string nt, std::unordered_set<std::string>&& attributes) {
            this->attributesTable[nt].merge(attributes);
        }

        void AddProduction(std::string&& lhs, std::vector<std::string>&& rhs) {
            this->productions[lhs].push_back(rhs);
        }

        void BuildParsingTable() {
            this->model->actionTable.clear();
            this->model->gotoTable.clear();
            this->model->productions.clear();

            for (auto& production : this->productions)
                for (auto& rhs : production.second)
                    this->model->productions.push_back({ std::string(production.first), std::vector<std::string>(rhs) });

            this->Items();

            for (int i = 0; i < (int)this->C.size(); ++i) {
                for (auto& it : this->C[i]) {
                    if (it.dot < it.rhs.size()) {
                        Symbol a = it.rhs[it.dot];
                        if (terminals.count(a))
                            this->model->SetAction(i, a, { Action::Type::Shift, this->FindState(this->GotoFunction(this->C[i], a)) });
                    } else if (it.lhs == this->𝑆 + "'")
                        this->model->SetAction(i, "EndOfFile", { Action::Type::Accept, 0 });
                    else
                        this->model->SetAction(i, it.lookahead, { Action::Type::Reduce, this->FindProductionIndex(it.lhs, it.rhs) });
                }
                for (auto& A : nonTerminals) {
                    auto gotoSet = this->GotoFunction(C[i], A);
                    if (!gotoSet.empty())
                        this->model->SetGoto(i, A, this->FindState(gotoSet));
                }
            }
            this->model->attributesTable = this->attributesTable;
        }
    };

private:
    Lexer                   lexer;
    std::shared_ptr<Model>  model;

public:
    Parser(Lexer&& lexer, std::shared_ptr<Model>&& model) : lexer(std::move(lexer)), model(std::move(model)) {}

    Tree Parse() {
        using Pack = std::vector<Tree::Node>;

        std::stack<std::variant<Tree::Node, Pack>> nodeStack;
        std::stack<int> stateStack;

        stateStack.push(0);

        Tree::Node bufferNode;
        Lexer::Token currentToken = this->lexer.GetNextToken();
        Symbol currentSym;
        Action act;
        Production p;

        while (true) {
            currentSym = this->model->classifier(currentToken);

            while (true) {
                act = this->model->GetAction(stateStack.top(), currentSym);

                if (Action::Type::Shift == act.type) {
                    stateStack.push(act.value);
                    nodeStack.push(this->model->tokenNodifier(currentToken));
                    currentToken = this->lexer.GetNextToken();

                    break;
                } else if (Action::Type::Reduce == act.type) {
                    p = this->model->productions[act.value];

                    bufferNode.label = this->model->NonTerminalHasAttribute(p.lhs, "soluble") ? std::string() : p.lhs;

                    for (SizeType i = 0; i < (SizeType)p.rhs.size(); i++) {
                        if (std::holds_alternative<Tree::Node>(nodeStack.top()))
                            bufferNode.children.push_back(std::move(std::get<Tree::Node>(nodeStack.top())));
                        else for (auto& node : std::get<Pack>(nodeStack.top()))
                            bufferNode.children.push_back(std::move(node));

                        nodeStack.pop();
                        stateStack.pop();
                    }

                    stateStack.push(this->model->GetGoto(stateStack.top(), p.lhs));

                    if (bufferNode.label.empty())
                        nodeStack.push(std::move(bufferNode.children));
                    else {
                        std::reverse(bufferNode.children.begin(), bufferNode.children.end());

                        nodeStack.push(std::move(bufferNode));
                    }
                } else if (Action::Type::Accept == act.type) {
                    return { .root = std::move(std::get<Tree::Node>(nodeStack.top())) };
                } else throw std::runtime_error("Parsing error");
            }
        }
    }
};

template<typename T>
class LocalUniqueNumberGenerator {
    T   accumulator;

public:
    LocalUniqueNumberGenerator() : accumulator() {}

    T operator()() {
        return accumulator++;
    }
};

template<typename T>
class LocalUniqueIdentifierGenerator {
    LocalUniqueNumberGenerator<T>   gen;
    std::string                     salt;

public:
    LocalUniqueIdentifierGenerator(std::string&& salt) : salt(std::move(salt)) {}

    std::string operator()() {
        return salt + std::to_string(this->gen());
    }
};

// template<> TODO
// class Serializer<Parser::Model> {
// public:
//     static void Serialize(const Parser::Model& model, std::ostream& os) {
//         throw std::runtime_error(std::format("TODO: {}:{}", __FILE__, __LINE__));
//     }

//     static Parser::Model Deserialize(std::istream& is) {
//         throw std::runtime_error(std::format("TODO: {}:{}", __FILE__, __LINE__));
//     }
// };

template<>
class Serializer<Parser::Grammar> {
public:
    static void Serialize(const Parser::Grammar& grammar, std::ostream& os) {
        throw std::runtime_error(std::format("TODO: {}:{}", __FILE__, __LINE__));
    }

    static Parser::Grammar Deserialize(const std::istream& is) {
        class Parser {
        public:
            struct Tree {
                struct Metadata {
                    std::string                     identifier;
                    std::unordered_set<std::string> attributes;
                };

                struct Required {
                    std::unordered_set<std::string> terminals;
                    std::unordered_set<std::string> nonTerminals;
                };

                struct Node {
                    enum class Type {
                        Sequence,
                        Alternative,
                        Terminal,
                        NonTerminal,
                        Group,
                        Optional,
                        Repetition
                    };

                    Type                                            type;
                    std::variant<std::string, std::vector<Node>>    data;
                };

                Metadata    metadata;
                Required    required;
                Node        root;
            };

        private:
            Lexer                       lexer;
            Lexer::Token                token;
            std::deque<Lexer::Token>    ahead;

            Tree::Required productionRequired;

            void Consume() {
                if (this->ahead.empty())
                    this->token = lexer.GetNextToken();
                else {
                    this->token = std::move(this->ahead.front());
                    this->ahead.pop_front();
                }
            }

            void LookAhead() {
                this->ahead.push_back(this->lexer.GetNextToken());
            }

            void Expect(Lexer::Token::Type expected) {
                if (expected != this->token.metadata.type)
                    throw std::runtime_error(std::format("Unexpected token \"{}\"", this->token.data));
            }

            void Expect(std::string expected) {
                if (expected != this->token.data)
                    throw std::runtime_error(std::format("Unexpected token \"{}\"", this->token.data));
            }

            Tree::Node ParseTerminal() {
                std::string data = std::move(this->token.data.substr(1, this->token.data.size() - 2));

                this->Consume();

                this->productionRequired.terminals.insert(data);

                return  { Tree::Node::Type::Terminal, std::move(data) };
            }

            Tree::Node ParseGroup() {
                this->Consume();

                Tree::Node node = this->ParseSequence([this]() -> bool {
                    return Lexer::Token::Type::EndOfFile != this->token.metadata.type && ')' != this->token.data[0];
                });

                this->Expect(")");
                this->Consume();

                return { Tree::Node::Type::Group, std::vector<Tree::Node>{ std::move(node) } };
            }

            Tree::Node ParseNonTerminal() {
                this->Consume();

                this->Expect(Lexer::Token::Type::Identifier);

                std::string data = std::move(this->token.data);

                this->Consume();

                this->Expect(">");
                this->Consume();

                this->productionRequired.nonTerminals.insert(data);

                return  { Tree::Node::Type::NonTerminal, std::move(data) };
            }

            Tree::Node ParseOptional() {
                this->Consume();

                Tree::Node node = this->ParseFactor();

                this->Expect("]");
                this->Consume();

                return  { Tree::Node::Type::Optional, std::vector<Tree::Node>{ std::move(node) } };
            }

            Tree::Node ParseRepeator() {
                this->Consume();

                std::vector<Tree::Node> sequence;

                while (Lexer::Token::Type::EndOfFile != this->token.metadata.type && '}' != this->token.data[0])
                    sequence.push_back(this->ParseFactor());

                if (sequence.empty())
                    throw std::runtime_error(std::format("TODO: {}:{}", __FILE__, __LINE__));

                this->Consume();

                return { Tree::Node::Type::Repetition, std::vector<Tree::Node>{ { Tree::Node::Type::Sequence, std::move(sequence) } } };
            }

            Tree::Node ParseFactor() {
                switch (this->token.data[0]) {
                    case '\"':
                        return this->ParseTerminal();
                    case '(':
                        return this->ParseGroup();
                    case '<':
                        return this->ParseNonTerminal();
                    case '[':
                        return this->ParseOptional();
                    case '{':
                        return this->ParseRepeator();

                    default:
                        std::cerr << this->token.data << std::endl;
                        throw std::runtime_error(std::format("TODO: {}:{}", __FILE__, __LINE__));
                }
            }

            Tree::Node ParseSequence(std::function<bool()> predicate) {
                std::vector<Tree::Node> buffer;
                std::vector<Tree::Node> alternatives;

                while (predicate()) {
                    if ('|' == this->token.data[0]) {
                        if (buffer.empty())
                            throw std::runtime_error(std::format("TODO: {}:{}", __FILE__, __LINE__));
                        alternatives.push_back(buffer.size() > 1 ? Tree::Node{ Tree::Node::Type::Sequence, std::move(buffer) } : std::move(buffer[0]));
                        buffer.clear();
                        this->Consume();
                    } else
                        buffer.push_back(this->ParseFactor());
                }

                if (buffer.empty())
                    throw std::runtime_error(std::format("TODO: {}:{}", __FILE__, __LINE__));

                if (alternatives.empty())
                    return buffer.size() > 1 ? Tree::Node{ Tree::Node::Type::Sequence, std::move(buffer) } : std::move(buffer[0]);

                alternatives.push_back(buffer.size() > 1 ? Tree::Node{ Tree::Node::Type::Sequence, std::move(buffer) } : std::move(buffer[0]));

                return Tree::Node{ Tree::Node::Type::Alternative, std::move(alternatives) };
            }

            Tree ParseProduction() {
                std::unordered_set<std::string> attributes;

                while ('@' == this->token.data[0]) {
                    this->Consume();

                    this->Expect(Lexer::Token::Type::Identifier);
                    attributes.insert(this->token.data);

                    this->Consume();
                }

                this->Expect("<");
                std::string lhs = std::get<std::string>(this->ParseNonTerminal().data);

                this->Expect("::=");
                this->Consume();

                this->productionRequired.terminals.clear();
                this->productionRequired.nonTerminals.clear();

                Tree::Node root = this->ParseSequence([this]() -> bool {
                    if (Lexer::Token::Type::EndOfFile == this->token.metadata.type)
                        return false;
                    
                    if ('<' == this->token.data[0]) {
                        assert(this->ahead.empty());

                        this->LookAhead();
                        this->LookAhead();
                        this->LookAhead();

                        return "::=" != this->ahead.back().data;
                    }

                    return '@' != this->token.data[0];
                });

                return { { std::move(lhs), std::move(attributes) }, std::move(this->productionRequired), std::move(root) };
            }

        public:
            Parser(const std::istream& is) : lexer(std::make_unique<std::istream>(is.rdbuf()), std::make_shared<PrefixTree>(), std::make_shared<PrefixTree>()) {
                lexer.GetPunctuation()->Insert({ "(", ")", "<", ">", "@", "[", "]", "{", "|", "}" });
                this->Consume();
            }

            std::vector<Tree> Parse() {
                std::vector<Tree> buffer;

                while (Lexer::Token::Type::EndOfFile != this->token.metadata.type)
                    buffer.push_back(this->ParseProduction());

                return buffer;
            }
        };

        class Expansor {
        public:
            using Expansion = std::vector<std::vector<std::string>>;

        private:
            using HelpersMap = std::map<std::set<std::vector<std::string>>, std::string>;
            using HelpersMapKey = std::set<std::vector<std::string>>;

            static Expansion Combine(const Expansion& a, const Expansion& b) {
                Expansion result;

                for (const auto& seqA : a)
                    for (const auto& seqB : b) {
                        std::vector<std::string> combined = seqA;
                        combined.insert(combined.end(), seqB.begin(), seqB.end());
                        result.push_back(combined);
                    }

                return result;
            }

            LocalUniqueIdentifierGenerator<std::uint32_t>               uidGen;
            HelpersMap                                                  helpersMap;
            std::vector<std::pair<Parser::Tree::Metadata, Expansion>>   helpers;

        public:
            std::vector<std::pair<Parser::Tree::Metadata, Expansion>>& GetHelpers() {
                return this->helpers;
            }

            Expansor() : uidGen("RepeaterHelper") {}

            Expansion Expand(const Parser::Tree::Node& node) {
                switch (node.type) {
                    case Parser::Tree::Node::Type::Terminal:
                    case Parser::Tree::Node::Type::NonTerminal:
                        return { { std::get<std::string>(node.data) } };

                    case Parser::Tree::Node::Type::Group:
                        return this->Expand(std::get<std::vector<Parser::Tree::Node>>(node.data)[0]);

                    case Parser::Tree::Node::Type::Optional: {
                        Expansion childExp = this->Expand(std::get<std::vector<Parser::Tree::Node>>(node.data)[0]);
                        childExp.push_back({});
                        std::reverse(childExp.begin(), childExp.end());
                        return childExp;
                    }

                    case Parser::Tree::Node::Type::Repetition: {
                        std::pair<Parser::Tree::Metadata, Expansion> helper;

                        Expansion expansion = this->Expand(std::get<std::vector<Parser::Tree::Node>>(node.data)[0]);

                        HelpersMapKey key;
                        std::string identifier;

                        for (auto& a : expansion)
                            key.insert(a);

                        auto it = this->helpersMap.find(key);

                        if (this->helpersMap.end() != it)
                            identifier = it->second;
                        else {
                            identifier = this->uidGen();

                            helper.first.identifier = identifier;
                            helper.first.attributes.insert("soluble");
                            helper.first.attributes.insert("abstractable");
                            helper.second = Expansor::Combine({ {}, { identifier } }, expansion);

                            helpersMap[key] = identifier;
                            this->helpers.push_back(std::move(helper));
                        }

                        return { { std::move(identifier) } };
                    }

                    case Parser::Tree::Node::Type::Sequence: {
                        Expansion result = {{}};

                        for (const auto& child : std::get<std::vector<Parser::Tree::Node>>(node.data))
                            result = Expansor::Combine(result, this->Expand(child));

                        return result;
                    }

                    case Parser::Tree::Node::Type::Alternative: {
                        Expansion result;

                        for (const auto& child : std::get<std::vector<Parser::Tree::Node>>(node.data)) {
                            Expansion childExp = this->Expand(child);
                            result.insert(result.end(), childExp.begin(), childExp.end());
                        }

                        return result;
                    }

                    default:
                        throw std::runtime_error(std::format("TODO: {}:{}", __FILE__, __LINE__));
                }
            }
        };

        auto productions = Parser(is).Parse();

        std::queue<std::size_t> queue;
        std::unordered_set<std::string> required;

        std::unordered_map<std::string, std::size_t> table;

        for (std::size_t i = 0, l = productions.size(); i < l; i++) {
            table[productions[i].metadata.identifier] = i;

            if (productions[i].metadata.attributes.contains("start")) {
                required.insert(productions[i].metadata.identifier);
                queue.push(i);
            }
        }

        if (required.empty())
            throw std::runtime_error("None production with start attribute");

        if (1 < required.size())
            throw std::runtime_error("Multiple productions with start attribute");

        ::Parser::Grammar grammar(*required.begin());
        Expansor expansor;

        std::size_t i;
        std::unordered_set<std::string> terminals;
        std::unordered_map<std::string, std::unordered_set<std::string>> nonTerminals;

        do {
            i = queue.front();
            queue.pop();

            auto expanded = expansor.Expand(productions[i].root);

            terminals.merge(productions[i].required.terminals);

            nonTerminals[productions[i].metadata.identifier].merge(productions[i].metadata.attributes);

            for (auto& path : expanded)
                grammar.AddProduction(std::string(productions[i].metadata.identifier), std::move(path));

            for (auto& nonTerminal : productions[i].required.nonTerminals)
                if (!required.contains(nonTerminal)) {
                    if (!table.contains(nonTerminal))
                        throw std::runtime_error(std::format("TODO: {}:{}", __FILE__, __LINE__));
                    queue.push(table.at(nonTerminal));
                    required.insert(nonTerminal);
                }
        } while (!queue.empty());

        for (const auto& nonTerminal : nonTerminals)
            if (terminals.contains(nonTerminal.first))
                throw std::runtime_error(std::format("TODO: {}:{}", __FILE__, __LINE__));

        auto helpers = expansor.GetHelpers();

        for (auto& helper : helpers) {
            nonTerminals[helper.first.identifier].merge(helper.first.attributes);

            for (auto& path : helper.second)
                grammar.AddProduction(std::string(helper.first.identifier), std::move(path));
        }

        terminals.erase("EndOfFile");
        terminals.erase("Identifier");
        terminals.erase("Literal");

        for (auto& terminal : terminals)
            grammar.AddTerminal(std::move(terminal));

        for (auto& nonTerminal : nonTerminals) {
            grammar.AddNonTerminal(nonTerminal.first);
            if (!nonTerminal.second.empty())
                grammar.AddAttributes(nonTerminal.first, std::move(nonTerminal.second));
        }

        grammar.BuildParsingTable();

        return grammar;
    }
};

int main(int argc, const char** argv) {
    std::setlocale(LC_ALL, "en_US.UTF-8");

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

    auto grammar = Serializer<Parser::Grammar>::Deserialize(std::ifstream(argv[1]));

    Parser parser(Lexer(std::make_unique<std::ifstream>(argv[2]), std::move(keywords), std::move(punctuation)), grammar.GetModel());

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