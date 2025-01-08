#include <iostream>
#include <vector>
#include <string>
#include <sstream>

class SExp {
public:
    virtual ~SExp() = default;
    virtual std::string toString() {
        return "SExp";
    }
};

class Atom : public SExp {
public:
    std::string value;
    Atom(std::string value) : value(value) {}
    std::string toString() override {
        return value;
    }
};

class SList : public SExp {
public:
    std::vector<SExp*> children;
    SList(std::vector<SExp*> children) : children(children) {}
    std::string toString() override {
        std::string result = "(";
        for (SExp* child : children) {
            result += child->toString();
        }
        result += ")";
        return result;
    }
    ~SList() {
        for (SExp* child : children) {
            delete child;
        }
    }
};

std::vector<std::string> tokenize(const std::string& input) {
    std::string stringWithSpaces = input;
    size_t pos;
    while ((pos = stringWithSpaces.find("(")) != std::string::npos) {
        stringWithSpaces.replace(pos, 1, " ( ");
    }
    while ((pos = stringWithSpaces.find(")")) != std::string::npos) {
        stringWithSpaces.replace(pos, 1, " ) ");
    }

    std::istringstream iss(stringWithSpaces);
    std::vector<std::string> tokens;
    std::string token;
    while (iss >> token) {
        tokens.push_back(token);
    }
    return tokens;
}

SExp* parseTokens(std::vector<std::string>& tokens) {
    if (tokens.empty()) {
        throw std::invalid_argument("Unexpected end of input");
    }

    std::string token = tokens[0];
    tokens.erase(tokens.begin());
    if (token == "(") {
        std::vector<SExp*> children;
        while (!tokens.empty() && tokens[0] != ")") {
            children.push_back(parseTokens(tokens));
        }
        if (tokens.empty()) {
            throw std::invalid_argument("Unmatched (");
        }
        tokens.erase(tokens.begin()); // Remove ")"
        return new SList(children);
    } else if (token == ")") {
        throw std::invalid_argument("Unmatched )");
    } else {
        return new Atom(token);
    }
}

SExp* parse(const std::string& input) {
    std::vector<std::string> tokens = tokenize(input);
    return parseTokens(tokens);
}

int main() {
    std::string input = "(+ 1 (* 2 3))";
    SExp* result = parse(input);
    std::cout << result->toString() << std::endl;
    delete result;
    return 0;
}
