#pragma once

#include "Utilities.hpp"

#include <cstddef>
#include <cstdint>
#include <memory>
#include <unordered_map>

namespace GenericStudyCompiler {
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

        bool LoadNextLine(Context& context);

        void HandleStringLiteral(Context& context);

        void HandleCharacterLiteral(Context& context);

    public:
        std::shared_ptr<PrefixTree> GetKeywords() noexcept;

        std::shared_ptr<PrefixTree> GetPunctuation() noexcept;

        Lexer(std::unique_ptr<std::istream>&& stream, std::shared_ptr<PrefixTree>&& keywords, std::shared_ptr<PrefixTree>&& punctuation);

        Token GetNextToken();
    };
}