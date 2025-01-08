class SExp {
    toString() {
        return 'SExp';
    }
}

class Atom extends SExp {
    constructor(value) {
        super();
        this.value = value;
    }

    toString() {
        return this.value;
    }
}

class SList extends SExp {
    constructor(children) {
        super();
        this.children = children;
    }

    toString() {
        return (
            '(' + this.children.map((child) => child.toString()).join('') + ')'
        );
    }
}

function tokenize(input) {
    return input
        .replaceAll('(', ' ( ')
        .replaceAll(')', ' ) ')
        .split(' ')
        .filter((token) => token.length > 0);
}

function parse(input) {
    const tokens = tokenize(input);
    return parseTokens(tokens);
}

function parseTokens(tokens) {
    if (tokens.length === 0) {
        throw new Error('Unexpected end of input');
    }

    const token = tokens.shift();
    if (token === '(') {
        const children = [];
        while (tokens.length > 0 && tokens[0] !== ')') {
            children.push(parseTokens(tokens));
        }
        if (tokens.length === 0) {
            throw new Error('Unmatched (');
        }
        tokens.shift(); // Remove ')'
        return new SList(children);
    } else if (token === ')') {
        throw new Error('Unmatched )');
    } else {
        return new Atom(token);
    }
}

const input = '(+ 1 (* 2 3))';
const result = parse(input);
console.log(result.toString());
