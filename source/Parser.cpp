#include "Parser.hpp"

#include <algorithm>
#include <cassert>
#include <format>
#include <queue>
#include <stack>
#include <stdexcept>
#include <variant>

namespace GenericStudyCompiler {
    void Parser::Model::SetAction(State state, std::string symbol, Action action) {
        this->actionTable[{ state, symbol }] = action;
    }

    void Parser::Model::RemoveAction(State state, std::string symbol) {
        this->actionTable.erase({ state, symbol });
    }

    bool Parser::Model::HasAction(State state, std::string symbol) const {
        return this->actionTable.end() == this->actionTable.find({ state, symbol });
    }

    Parser::Action Parser::Model::GetAction(State state, std::string symbol) const {
        auto it = this->actionTable.find({ state, symbol });
        if (this->actionTable.end() == it)
            throw std::runtime_error(std::format("Action ({}:\"{}\") not found", state, symbol));
        return it->second;
    }

    void Parser::Model::SetGoto(State state, std::string symbol, State target) {
        this->gotoTable[{ state, symbol }] = target;
    }

    void Parser::Model::RemoveGoto(State state, std::string symbol) {
        this->gotoTable.erase({ state, symbol });
    }

    bool Parser::Model::HasGoto(State state, std::string symbol) const {
        return this->gotoTable.end() == this->gotoTable.find({ state, symbol });
    }

    Parser::State Parser::Model::GetGoto(State state, std::string symbol) const {
        auto it = this->gotoTable.find({ state, symbol });
        if (this->gotoTable.end() == it)
            throw std::runtime_error(std::format("Goto ({}:\"{}\") not found", state, symbol));
        return it->second;
    }

    bool Parser::Model::NonTerminalHasAttribute(std::string nonTerminal, std::string attribute) const noexcept {
        if (this->attributesTable.contains(nonTerminal))
            return this->attributesTable.at(nonTerminal).contains(attribute);
        return false;
    }

    bool Parser::Grammar::NonTerminalHasAttribute(std::string nonTerminal, std::string attribute) const noexcept {
        if (this->attributesTable.contains(nonTerminal))
            return this->attributesTable.at(nonTerminal).contains(attribute);
        return false;
    }

    void Parser::Grammar::ComputeFirstSets() {
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

    std::set<Parser::Symbol> Parser::Grammar::First(const std::vector<Symbol>& symbols) const {
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

    std::set<Parser::Grammar::Item> Parser::Grammar::Closure(const std::set<Item>& I) const {
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

    std::set<Parser::Grammar::Item> Parser::Grammar::GotoFunction(const std::set<Item>& I, const Symbol& X) const {
        std::set<Item> J;
        for (auto& it : I)
            if (it.dot < it.rhs.size() && it.rhs[it.dot] == X)
                J.insert({ it.lhs, it.rhs, it.dot + 1, it.lookahead });
        return this->Closure(J);
    }

    void Parser::Grammar::Items() {
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

    int Parser::Grammar::FindState(const std::set<Item>& I) const {
        for (int i = 0; i < (int)this->C.size(); ++i)
            if (this->C[i] == I)
                return i;
        return -1;
    }

    int Parser::Grammar::FindProductionIndex(const Symbol& lhs, const std::vector<Symbol>& rhs) const {
        for (int i = 0; i < (int)this->model->productions.size(); ++i)
            if (this->model->productions[i].lhs == lhs && this->model->productions[i].rhs == rhs)
                return i;
        return -1;
    }

    std::shared_ptr<Parser::Model> Parser::Grammar::GetModel() noexcept { return this->model; }

    Parser::Grammar::Grammar(const std::string& 𝑆) : model(std::make_shared<Model>()), 𝑆(𝑆) {
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

        this->model->tokenNodifier = [](Lexer::Token& token) -> ConcreteSyntaxTree::Node {
            ConcreteSyntaxTree::Node node;

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

            node.range = token.metadata.range;

            return node;
        };
    }

    void Parser::Grammar::AddTerminal(std::string t) {
        this->terminals.insert(t);
    }

    void Parser::Grammar::AddNonTerminal(std::string nt) {
        this->nonTerminals.insert(nt);
    }

    void Parser::Grammar::AddAttributes(std::string nt, std::unordered_set<std::string>&& attributes) {
        this->attributesTable[nt].merge(attributes);
    }

    void Parser::Grammar::AddProduction(std::string&& lhs, std::vector<std::string>&& rhs) {
        this->productions[lhs].push_back(rhs);
    }

    void Parser::Grammar::BuildParsingTable() {
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

    Parser::Parser(Lexer&& lexer, std::shared_ptr<Model>&& model) : lexer(std::move(lexer)), model(std::move(model)) {}

    ConcreteSyntaxTree Parser::Parse() {
        using Pack = std::vector<ConcreteSyntaxTree::Node>;

        std::stack<std::variant<ConcreteSyntaxTree::Node, Pack>> nodeStack;
        std::stack<int> stateStack;

        stateStack.push(0);

        ConcreteSyntaxTree::Node bufferNode;
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
                        if (std::holds_alternative<ConcreteSyntaxTree::Node>(nodeStack.top()))
                            bufferNode.children.push_back(std::move(std::get<ConcreteSyntaxTree::Node>(nodeStack.top())));
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

                        bufferNode.range = { bufferNode.children.front().range.start, bufferNode.children.back().range.end };

                        nodeStack.push(std::move(bufferNode));
                    }
                } else if (Action::Type::Accept == act.type) {
                    return { .root = std::move(std::get<ConcreteSyntaxTree::Node>(nodeStack.top())) };
                } else throw std::runtime_error("Parsing error");
            }
        }
    }

    void Serializer<Parser::Grammar>::Serialize(const Parser::Grammar& grammar, std::ostream& os) {
        throw std::runtime_error(std::format("TODO: {}:{}", __FILE__, __LINE__));
    }

    Parser::Grammar Serializer<Parser::Grammar>::Deserialize(const std::istream& is) {
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

        GenericStudyCompiler::Parser::Grammar grammar(*required.begin());
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
}