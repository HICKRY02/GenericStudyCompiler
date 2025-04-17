<BuiltinType> ::= "bool" | "char" | "double" | "float" | "int" | "long" | "short" | "void"

@soluble
<Declaration> ::= <NamespaceDeclaration> | <VariableDeclaration>

<Declarations> ::= [{ <Declaration> }]

<DeclarationsBlock> ::= "{" <Declarations> "}"

@soluble
<Instance> ::= "Identifier"

<Instances> ::= <Instance> [{"," <Instance>}]

<NamespaceDeclaration> ::= "namespace" "Identifier" <DeclarationsBlock>

@start
<Program> ::= <Declarations>

<Type> ::= ["const"] (<BuiltinType> | "Identifier")

<VariableDeclaration> ::= <Type> <Instances> ";"