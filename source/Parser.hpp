#pragma once

#include "ConcreteSyntaxTree.hpp"
#include "Lexer.hpp"
#include "Serializer.hpp"

#include <functional>
#include <map>
#include <set>
#include <string>
#include <unordered_set>
#include <vector>

namespace GenericStudyCompiler {
    class Parser {
    public:
        using SizeType      = std::int32_t;
        using State         = std::int32_t;
        using Symbol        = std::string;

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
            using TokenNodifier         = std::function<ConcreteSyntaxTree::Node(Lexer::Token&)>;
            using AttributeTable        = std::unordered_map<Symbol, std::unordered_set<std::string>>;

            ActionTable      actionTable;
            ProductionTable  productions;
            GotoTable        gotoTable;
            TokenClassifier  classifier;
            TokenNodifier    tokenNodifier;
            AttributeTable   attributesTable;

            void SetAction(State state, std::string symbol, Action action);

            void RemoveAction(State state, std::string symbol);

            bool HasAction(State state, std::string symbol) const;

            Action GetAction(State state, std::string symbol) const;

            void SetGoto(State state, std::string symbol, State target);

            void RemoveGoto(State state, std::string symbol);

            bool HasGoto(State state, std::string symbol) const;

            State GetGoto(State state, std::string symbol) const;

            bool NonTerminalHasAttribute(std::string nonTerminal, std::string attribute) const noexcept;
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

            bool NonTerminalHasAttribute(std::string nonTerminal, std::string attribute) const noexcept;

            void ComputeFirstSets();
        
            std::set<Symbol> First(const std::vector<Symbol>& symbols) const;
        
            std::set<Item> Closure(const std::set<Item>& I) const;
        
            std::set<Item> GotoFunction(const std::set<Item>& I, const Symbol& X) const;
        
            void Items();

            int FindState(const std::set<Item>& I) const;
        
            int FindProductionIndex(const Symbol& lhs, const std::vector<Symbol>& rhs) const;

        public:
            std::shared_ptr<Model> GetModel() noexcept;

            Grammar(const std::string& 𝑆);

            void AddTerminal(std::string t);

            void AddNonTerminal(std::string nt);

            void AddAttributes(std::string nt, std::unordered_set<std::string>&& attributes);

            void AddProduction(std::string&& lhs, std::vector<std::string>&& rhs);

            void BuildParsingTable();
        };

    private:
        Lexer                   lexer;
        std::shared_ptr<Model>  model;

    public:
        Parser(Lexer&& lexer, std::shared_ptr<Model>&& model);

        ConcreteSyntaxTree Parse();
    };

    // template<> TODO
    // class Serializer<Parser::Model> {
    // public:
        // static void Serialize(const Parser::Model& model, std::ostream& os) {
            // throw std::runtime_error(std::format("TODO: {}:{}", __FILE__, __LINE__));
        // }

        // static Parser::Model Deserialize(std::istream& is) {
            // throw std::runtime_error(std::format("TODO: {}:{}", __FILE__, __LINE__));
        // }
    // };

    template<>
    class Serializer<Parser::Grammar> {
    public:
        static void Serialize(const Parser::Grammar& grammar, std::ostream& os);

        static Parser::Grammar Deserialize(const std::istream& is);
    };
}