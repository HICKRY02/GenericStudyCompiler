#include <array>
#include <initializer_list>
#include <string>
#include <string_view>
#include <unordered_map>

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

bool IsBinary(char c);

bool IsOctogonal(char c);

class PrefixTree {
public:
    class Node {
        mutable std::unordered_map<char, Node>  children;
        bool                                    isEndOfString = false;

    public:
        bool IsEndOfString() const noexcept;

        bool HasChildren() const noexcept;

        void Insert(std::string_view str);

        Node* Search(char c);

        const Node* Search(char c) const;
    };

private:
    Node root;

public:
    Node& GetRoot() noexcept;

    const Node& GetRoot() const noexcept;

    void Insert(std::string_view str);

    void Insert(std::initializer_list<std::string_view> list);

    bool Search(std::string_view str) const;
};

std::string GreedyMatching(std::string_view str, const PrefixTree& pattern);

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