class SExp:
    def __str__(self):
        return "SExp"

class Atom(SExp):
    def __init__(self, value):
        self.value = value
    
    def __str__(self):
        return str(self.value)

class SList(SExp):
    def __init__(self, children):
        self.children = children

    def __str__(self):
        return "(" + "".join(str(child) for child in self.children) + ")"

def tokenize(input_string):
    input_string = input_string.replace("(", " ( ").replace(")", " ) ")
    return [token for token in input_string.split() if token]

def parse(input_string):
    tokens = tokenize(input_string)
    return parse_tokens(tokens)

def parse_tokens(tokens):
    if not tokens:
        raise ValueError("Unexpected end of input")
    
    token = tokens.pop(0)
    if token == "(":
        children = []
        while tokens and tokens[0] != ")":
            children.append(parse_tokens(tokens))
        if not tokens:
            raise ValueError("Unmatched (")
        tokens.pop(0) # Remove ")"
        return SList(children)
    elif token == ")":
        raise ValueError("Unmatched )")
    else:
        return Atom(token)

if __name__ == "__main__":
    input_string = "(+ 1 (* 2 3))"    
    result = parse(input_string)
    print(result)
