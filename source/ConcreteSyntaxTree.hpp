#pragma once

#include "Utilities.hpp"

#include <string>
#include <vector>

namespace GenericStudyCompiler {
    struct ConcreteSyntaxTree {
        struct Node {
            std::vector<Node>           children;
            std::string                 label;
            Range<Point<std::size_t>>   range;
        };

        Node root;
    };
}