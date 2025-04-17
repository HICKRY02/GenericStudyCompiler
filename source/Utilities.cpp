#include "Utilities.hpp"

bool IsBinary(char c) {
    return '0' == c || '1' == c;
}

bool IsOctogonal(char c) {
    return '0' <= c && '7' >= c;
}

bool PrefixTree::Node::IsEndOfString() const noexcept { return this->isEndOfString; }

bool PrefixTree::Node::HasChildren() const noexcept { return !this->children.empty(); }

void PrefixTree::Node::Insert(std::string_view str) {
    Node* current = this;

    for (char c : str)
        current = &current->children.try_emplace(c, Node()).first->second;

    current->isEndOfString = true;
}

PrefixTree::Node* PrefixTree::Node::Search(char c) {
    std::unordered_map<char, Node>::iterator iterator = this->children.find(c);

    if (this->children.end() != iterator)
        return &iterator->second;

    return nullptr;
}

const PrefixTree::Node* PrefixTree::Node::Search(char c) const {
    std::unordered_map<char, Node>::iterator iterator = this->children.find(c);

    if (this->children.end() != iterator)
        return &iterator->second;

    return nullptr;
}

PrefixTree::Node& PrefixTree::GetRoot() noexcept {
    return this->root;
}

const PrefixTree::Node& PrefixTree::GetRoot() const noexcept {
    return this->root;
}

void PrefixTree::Insert(std::string_view str) {
    root.Insert(str);
}

void PrefixTree::Insert(std::initializer_list<std::string_view> list) {
    for (const auto& str : list)
        root.Insert(str);
}

bool PrefixTree::Search(std::string_view str) const {
    const Node* current = &this->root;

    for (char c : str) {
        current = current->Search(c);

        if (!current)
            return false;
    }

    return current->IsEndOfString();
}

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