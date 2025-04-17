#include "Parser.hpp"

#include <fstream>
#include <locale>

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

    parser.Parse();

    return 0;
}