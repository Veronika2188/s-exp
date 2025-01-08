// Define the data type for S-expressions (Symbolic Expressions)
interface SExp {
    type: string;
    value: any;
    toString(): string;
}

function createAtom(value: string): SExp {
    return {
        type: 'atom',
        value: value,
        toString: () => value,
    };
}

function createList(children: SExp[]): SExp {
    return {
        type: 'list',
        value: children,
        toString: () =>
            '(' + children.map((sexp) => sexp.toString()).join('') + ')',
    };
}

// Tokenize the input string into a list of tokens
function tokenize(input: string): string[] {
    return input
        .replace('(', ' ( ')
        .replace(')', ' ) ')
        .split(' ')
        .filter((token) => token.length > 0);
}

// Parse a list of tokens into an S-expression
function parseTokens(tokens: string[]): SExp {
    if (tokens.length === 0) {
        throw new Error('Unexpected end of input');
    }

    const token = tokens.shift();
    if (token === '(') {
        const children: SExp[] = [];
        while (tokens.length > 0 && tokens[0] !== ')') {
            children.push(parseTokens(tokens));
        }
        if (tokens.length === 0) {
            throw new Error('Unmatched (');
        }
        tokens.shift(); // Remove ')'
        return createList(children);
    } else if (token === ')') {
        throw new Error('Unmatched )');
    } else if (typeof token === 'string') {
        return createAtom(token);
    } else {
        throw new Error('Invalid token type');
    }
}

// Parse an input string into an S-expression
function parse(input: string): SExp {
    return parseTokens(tokenize(input));
}

// Main function to test the parser
const input = '(+ 1 (* 2 3))';
const result = parse(input);
console.log(result.toString());
