/** * ==================================================================
 * 1. LEXER & PARSER
 * ==================================================================
 */
console.log("HE");
const TokenType = {
    KW_VAL: "val",
    KW_IF: "if",
    KW_ELSE: "else",
    KW_MATCH: "match",
    KW_CASE: "case",
    BOOL_LIT: "bool_lit",
    INT_LIT: "int_lit",
    IDENT: "ident",
    TYPE_INT: "Int",
    TYPE_BOOL: "Boolean",
    OP_PLUS: "+",
    OP_MINUS: "-",
    OP_MUL: "*",
    OP_DIV: "/",
    OP_MOD: "%",
    OP_GT: ">",
    OP_LT: "<",
    OP_LE: "<=", // FIXED: Added token type for <=
    OP_EQ: "==",
    OP_NOT: "!",
    OP_ASSIGN: "=",
    OP_ARROW: "=>",
    LPAREN: "(",
    RPAREN: ")",
    LBRACE: "{",
    RBRACE: "}",
    SEMI: ";",
    COMMA: ",",
    COLON: ":",
    EOF: "eof",
};

function tokenize(input) {
    let i = 0;
    const tokens = [];
    const keywords = {
        val: TokenType.KW_VAL,
        if: TokenType.KW_IF,
        else: TokenType.KW_ELSE,
        true: TokenType.BOOL_LIT,
        false: TokenType.BOOL_LIT,
        match: TokenType.KW_MATCH,
        case: TokenType.KW_CASE,
        Int: TokenType.TYPE_INT,
        Boolean: TokenType.TYPE_BOOL,
    };

    while (i < input.length) {
        const char = input[i];
        if (/\s/.test(char)) {
            i++;
            continue;
        }

        if (/[0-9]/.test(char)) {
            let num = "";
            while (i < input.length && /[0-9]/.test(input[i])) num += input[i++];
            tokens.push({ type: TokenType.INT_LIT, value: parseInt(num) });
            continue;
        }

        if (/[a-zA-Z]/.test(char)) {
            let id = "";
            while (i < input.length && /[a-zA-Z0-9]/.test(input[i])) id += input[i++];
            tokens.push({
                type: keywords[id] || TokenType.IDENT,
                value: id === "true" ? true : id === "false" ? false : id,
            });
            continue;
        }

        // FIXED: Correctly handle two-character operators
        const two = input.substr(i, 2);
        if (["==", "=>", "<=", ">="].includes(two)) {
            let type = TokenType.OP_EQ;
            if (two === "=>") type = TokenType.OP_ARROW;
            else if (two === "<=") type = TokenType.OP_LE; // FIXED: Map to OP_LE

            tokens.push({ type: type, value: two });
            i += 2;
            continue;
        }

        const singleMap = {
            "+": TokenType.OP_PLUS,
            "-": TokenType.OP_MINUS,
            "*": TokenType.OP_MUL,
            "/": TokenType.OP_DIV,
            "%": TokenType.OP_MOD,
            ">": TokenType.OP_GT,
            "<": TokenType.OP_LT,
            "!": TokenType.OP_NOT,
            "=": TokenType.OP_ASSIGN,
            "(": TokenType.LPAREN,
            ")": TokenType.RPAREN,
            "{": TokenType.LBRACE,
            "}": TokenType.RBRACE,
            ";": TokenType.SEMI,
            ",": TokenType.COMMA,
            ":": TokenType.COLON,
        };

        if (singleMap[char]) {
            tokens.push({ type: singleMap[char], value: char });
            i++;
            continue;
        }
        throw new Error(`Unexpected character: ${char}`);
    }
    return tokens;
}

class Parser {
    constructor(tokens) {
        this.tokens = tokens;
        this.pos = 0;
    }
    peek() {
        return this.tokens[this.pos] || { type: TokenType.EOF };
    }
    consume() {
        return this.tokens[this.pos++];
    }
    match(t) {
        if (this.peek().type === t) return this.consume();
        return null;
    }
    expect(t) {
        if (this.peek().type !== t)
            throw new Error(`Expected ${t}, found ${this.peek().type}`);
        return this.consume();
    }

    parse() {
        return this.parseExp();
    }

    parseExp() {
        return this.parseMatch();
    }

    parseMatch() {
        let left = this.parseComp();
        if (this.match(TokenType.KW_MATCH)) {
            this.expect(TokenType.LBRACE);
            this.expect(TokenType.KW_CASE);
            this.expect(TokenType.LPAREN);
            const vars = [];
            vars.push(this.expect(TokenType.IDENT).value);
            while (this.match(TokenType.COMMA)) {
                vars.push(this.expect(TokenType.IDENT).value);
            }
            this.expect(TokenType.RPAREN);
            this.expect(TokenType.OP_ARROW);
            const body = this.parseExp();
            this.expect(TokenType.RBRACE);
            return { type: "Match", target: left, vars, body };
        }
        return left;
    }

    parseComp() {
        let left = this.parseAdd();
        // FIXED: Added "<=" to the allowed comparison operators
        while (["==", "<", ">", "<="].includes(this.peek().value)) {
            const op = this.consume().value;
            left = { type: "BinOp", op, left, right: this.parseAdd() };
        }
        return left;
    }

    parseAdd() {
        let left = this.parseTerm();
        while (["+", "-"].includes(this.peek().value)) {
            const op = this.consume().value;
            left = { type: "BinOp", op, left, right: this.parseTerm() };
        }
        return left;
    }

    parseTerm() {
        let left = this.parseUnary();
        while (["*", "/", "%"].includes(this.peek().value)) {
            const op = this.consume().value;
            left = { type: "BinOp", op, left, right: this.parseUnary() };
        }
        return left;
    }

    parseUnary() {
        if (["-", "!"].includes(this.peek().value)) {
            const op = this.consume().value;
            const expr = this.parseUnary();
            return { type: "UnOp", op, expr };
        }
        return this.parseFactor();
    }

    parseFactor() {
        const t = this.peek();
        if (t.type === TokenType.INT_LIT)
            return { type: "IntLit", val: this.consume().value };
        if (t.type === TokenType.BOOL_LIT)
            return { type: "BoolLit", val: this.consume().value };
        if (t.type === TokenType.IDENT)
            return { type: "Var", name: this.consume().value };

        if (t.type === TokenType.LBRACE) {
            this.consume();
            if (this.peek().type === TokenType.KW_VAL) {
                const decl = this.parseDecl();
                this.expect(TokenType.SEMI);
                const body = this.parseExp();
                this.expect(TokenType.RBRACE);
                return { type: "Block", decl, body };
            }
            const e = this.parseExp();
            this.expect(TokenType.RBRACE);
            return e;
        }

        if (t.type === TokenType.KW_IF) {
            this.consume();
            this.expect(TokenType.LPAREN);
            const cond = this.parseExp();
            this.expect(TokenType.RPAREN);
            const cons = this.parseExp();
            this.expect(TokenType.KW_ELSE);
            const alt = this.parseExp();
            return { type: "If", cond, cons, alt };
        }

        if (t.type === TokenType.LPAREN) {
            this.consume();
            const e1 = this.parseExp();
            if (this.match(TokenType.COMMA)) {
                const elems = [e1];
                do {
                    elems.push(this.parseExp());
                } while (this.match(TokenType.COMMA));
                this.expect(TokenType.RPAREN);
                return { type: "Tuple", elems };
            }
            this.expect(TokenType.RPAREN);
            return e1;
        }
        throw new Error(`Unexpected token: ${t.type}`);
    }

    parseDecl() {
        this.expect(TokenType.KW_VAL);
        const name = this.expect(TokenType.IDENT).value;
        let typeAnno = null;
        if (this.match(TokenType.COLON)) typeAnno = this.parseType();
        this.expect(TokenType.OP_ASSIGN);
        const expr = this.parseExp();
        return { type: "ValDecl", name, typeAnno, expr };
    }

    parseType() {
        if (this.match(TokenType.TYPE_INT)) return "Int";
        if (this.match(TokenType.TYPE_BOOL)) return "Boolean";
        if (this.match(TokenType.LPAREN)) {
            const types = [];
            types.push(this.parseType());
            while (this.match(TokenType.COMMA)) {
                types.push(this.parseType());
            }
            this.expect(TokenType.RPAREN);
            return `(${types.join(", ")})`;
        }
        return "Unknown";
    }
}

/** * ==================================================================
 * 2. LOGIC (Type Check & Op Semantics)
 * ==================================================================
 */

function evalSimple(ast) {
    if (ast.type === "IntLit") return ast.val;
    if (ast.type === "BoolLit") return ast.val;
    if (ast.type === "Tuple") return ast.elems.map(evalSimple);
    if (ast.type === "UnOp" && ast.op === "-") return -evalSimple(ast.expr);
    if (ast.type === "UnOp" && ast.op === "!") return !evalSimple(ast.expr);
    throw new Error("Env only supports literals/tuples");
}

function inferTypeVal(val) {
    if (typeof val === "number") return "Int";
    if (typeof val === "boolean") return "Boolean";
    if (Array.isArray(val)) return `(${val.map(inferTypeVal).join(", ")})`;
    return "Unknown";
}

function exprToString(e, shorten) {
    if (!e) return "?";
    if (shorten) {
        if (e.type === "Block") return "{ ... }";
        if (e.type === "If") return "if ...";
        if (e.type === "Match") return "match ...";
        if (e.type === "Tuple") return "( ... )";
        if (e.type === "BinOp") {
            const l =
                e.left.type === "BinOp" ? "( ... )" : exprToString(e.left, true);
            const r =
                e.right.type === "BinOp" ? "( ... )" : exprToString(e.right, true);
            return `${l} ${e.op} ${r}`;
        }
    }

    if (e.type === "UnOp") return `${e.op}${exprToString(e.expr, shorten)}`;
    if (e.type === "Tuple")
        return `(${e.elems.map((x) => exprToString(x, shorten)).join(", ")})`;
    if (e.type === "IntLit") return e.val;
    if (e.type === "BoolLit") return e.val;
    if (e.type === "Var") return e.name;
    if (e.type === "BinOp")
        return `${exprToString(e.left, false)} ${e.op} ${exprToString(e.right, false)}`;
    if (e.type === "If")
        return `if (${exprToString(e.cond, false)}) ${exprToString(e.cons, false)} else ${exprToString(e.alt, false)}`;
    if (e.type === "Block") {
        const typeStr = e.decl.typeAnno ? `: ${e.decl.typeAnno}` : "";
        return `{ val ${e.decl.name}${typeStr} = ${exprToString(e.decl.expr, false)}; ${exprToString(e.body, false)} }`;
    }
    if (e.type === "Match")
        return `${exprToString(e.target, false)} match { case (${e.vars.join(", ")}) => ${exprToString(e.body, false)} }`;
    return "?";
}

const createNode = (rule, expr, res, children, env) => ({
    rule,
    expr,
    res,
    children,
    env,
    id: -1,
});

function deriveType(env, expr) {
    if (expr.type === "IntLit")
        return createNode("T-INTLIT", expr, "Int", [], env); // Pass env
    if (expr.type === "BoolLit")
        return createNode("T-BOOLLIT", expr, "Boolean", [], env);
    if (expr.type === "Var") {
        const t = env[expr.name];
        if (!t) throw new Error(`Type Error: ${expr.name} not found`);
        return createNode("T-IDENT", expr, t, [], env);
    }

    if (expr.type === "UnOp") {
        const d = deriveType(env, expr.expr);
        if (expr.op === "-") {
            if (d.res !== "Int") throw new Error("Unary minus requires Int");
            return createNode("T-INTNEG", expr, "Int", [d], env);
        } else if (expr.op === "!") {
            if (d.res !== "Boolean") throw new Error("Not requires Boolean");
            return createNode("T-NOT", expr, "Boolean", [d], env);
        }
    }

    if (expr.type === "BinOp") {
        const d1 = deriveType(env, expr.left);
        const d2 = deriveType(env, expr.right);
        let rule = "T-OP",
            res = "Int";
        if (["+", "-", "*", "/", "%"].includes(expr.op)) {
            if (d1.res !== "Int" || d2.res !== "Int")
                throw new Error("Arithmetic on non-Int");
            rule = "T-INT" + (expr.op === "+" ? "PLUS" : "OP");
        } else {
            if (d1.res !== "Int" || d2.res !== "Int")
                throw new Error("Compare on non-Int");
            rule = "T-COMP";
            res = "Boolean";
        }
        return createNode(rule, expr, res, [d1, d2], env);
    }

    if (expr.type === "If") {
        const dCond = deriveType(env, expr.cond);
        if (dCond.res !== "Boolean") throw new Error("If cond must be Boolean");
        const dCons = deriveType(env, expr.cons);
        const dAlt = deriveType(env, expr.alt);
        if (dCons.res !== dAlt.res) throw new Error("Branch types mismatch");
        return createNode("T-IF", expr, dCons.res, [dCond, dCons, dAlt], env);
    }

    if (expr.type === "Block") {
        const dDecl = deriveType(env, expr.decl.expr);
        if (expr.decl.typeAnno && expr.decl.typeAnno !== dDecl.res)
            throw new Error("Type annotation mismatch");

        const declNode = createNode(
            expr.decl.typeAnno ? "T-VALDECLANNO" : "T-VALDECL",
            { type: "Custom", str: `val ${expr.decl.name} = ...` },
            "θ", // Keep as is or pass 'env' if you want context here too
            [dDecl],
            env,
        );

        const newEnv = { ...env, [expr.decl.name]: dDecl.res };
        const dBody = deriveType(newEnv, expr.body);
        return createNode("T-BLOCK", expr, dBody.res, [declNode, dBody], env);
    }

    if (expr.type === "Tuple") {
        const kids = expr.elems.map((e) => deriveType(env, e));
        const tStr = `(${kids.map((k) => k.res).join(", ")})`;
        return createNode("T-TUPLE", expr, tStr, kids, env);
    }

    if (expr.type === "Match") {
        const dT = deriveType(env, expr.target);
        const inner = dT.res
            .replace(/^\(|\)$/g, "")
            .split(",")
            .map((s) => s.trim());

        // Note: Match arity check omitted for brevity, same as before
        const newEnv = { ...env };
        expr.vars.forEach((v, i) => (newEnv[v] = inner[i] || "Unknown"));
        const dB = deriveType(newEnv, expr.body);
        return createNode("T-MATCH", expr, dB.res, [dT, dB], env);
    }
    throw new Error("Unknown expr");
}

function deriveOp(rho, expr) {
    // REMOVED: const rhoKeys = ...

    if (expr.type === "IntLit")
        return createNode("INTLIT", expr, expr.val, [], rho); // Pass rho
    if (expr.type === "BoolLit")
        return createNode(expr.val ? "TRUE" : "FALSE", expr, expr.val, [], rho);
    if (expr.type === "Var") {
        const v = rho[expr.name];
        if (v === undefined) throw new Error(`${expr.name} undefined`);
        return createNode("IDENT", expr, v, [], rho);
    }

    if (expr.type === "UnOp") {
        const d = deriveOp(rho, expr.expr);
        let val, rule;
        if (expr.op === "-") {
            val = -d.res;
            rule = "INTNEG";
        }
        if (expr.op === "!") {
            val = !d.res;
            rule = "NOT";
        }
        return createNode(rule, expr, val, [d], rho);
    }

    if (expr.type === "BinOp") {
        const d1 = deriveOp(rho, expr.left);
        const d2 = deriveOp(rho, expr.right);
        const v1 = d1.res,
            v2 = d2.res;
        let res,
            rule = "BINOP";
        // ... (Your existing switch case for operations) ...
        switch (expr.op) {
            case "+":
                res = v1 + v2;
                rule = "INTPLUS";
                break;
            case "*":
                res = v1 * v2;
                rule = "INTMUL";
                break;
            case "-":
                res = v1 - v2;
                rule = "INTSUB";
                break;
            case "/":
                res = Math.trunc(v1 / v2);
                rule = "INTDIV";
                break;
            case "%":
                res = v1 % v2;
                rule = "INTMOD";
                break;
            case ">":
                res = v1 > v2;
                rule = res ? "GTTRUE" : "GTFALSE";
                break;
            case "<":
                res = v1 < v2;
                rule = res ? "LTTRUE" : "LTFALSE";
                break;
            case "<=":
                res = v1 <= v2;
                rule = res ? "LETRUE" : "LEFALSE";
                break;
            case "==":
                res = v1 === v2;
                rule = res ? "EQTRUE" : "EQFALSE";
                break;
        }
        return createNode(rule, expr, res, [d1, d2], rho);
    }

    if (expr.type === "If") {
        const dCond = deriveOp(rho, expr.cond);
        const branch = dCond.res ? expr.cons : expr.alt;
        const dBranch = deriveOp(rho, branch);
        return createNode(
            dCond.res ? "IFTRUE" : "IFFALSE",
            expr,
            dBranch.res,
            [dCond, dBranch],
            rho,
        );
    }

    if (expr.type === "Block") {
        const dDecl = deriveOp(rho, expr.decl.expr);
        const declNode = createNode(
            "VALDECL",
            { type: "Custom", str: `val ${expr.decl.name} = ...` },
            "ρ'", // Placeholder for inner declaration logic
            [dDecl],
            rho,
        );
        const newRho = { ...rho, [expr.decl.name]: dDecl.res };
        const dBody = deriveOp(newRho, expr.body);
        return createNode("BLOCK", expr, dBody.res, [declNode, dBody], rho);
    }

    if (expr.type === "Tuple") {
        const kids = expr.elems.map((e) => deriveOp(rho, e));
        return createNode(
            "TUPLE",
            expr,
            kids.map((k) => k.res),
            kids,
            rho,
        );
    }

    if (expr.type === "Match") {
        const dT = deriveOp(rho, expr.target);
        const newRho = { ...rho };
        expr.vars.forEach((v, i) => (newRho[v] = dT.res[i]));
        const dB = deriveOp(newRho, expr.body);
        return createNode("MATCH", expr, dB.res, [dT, dB], rho);
    }

    return createNode("UNKNOWN", expr, "?", [], rho);
}

/** * ==================================================================
 * 4. RENDERING & STATE MANAGEMENT
 * ==================================================================
 */
let appState = { typeTree: null, opTree: null, step: 0, parsed: false };

function assignIds(node, counter) {
    node.id = counter.val++;
    node.children.forEach((c) => assignIds(c, counter));
}

function resetState() {
    appState = { typeTree: null, opTree: null, step: 0, parsed: false };
    document.getElementById("typeOutput").innerHTML =
        '<div style="color:#888; font-style:italic">Ready to derive...</div>';
    document.getElementById("opOutput").innerHTML =
        '<div style="color:#888; font-style:italic">Ready to derive...</div>';
    document.getElementById("typeCounter").innerText = "";
    document.getElementById("opCounter").innerText = "";
}

function parseEnvInput() {
    const raw = document.getElementById("initialEnv").value.trim();
    const rho = {};
    const gamma = {};

    if (!raw) return { rho, gamma };

    const tokens = tokenize(raw);
    const parser = new Parser(tokens);

    while (parser.peek().type !== TokenType.EOF) {
        const nameToken = parser.expect(TokenType.IDENT);
        const name = nameToken.value;

        parser.expect(TokenType.OP_ASSIGN);

        const valAst = parser.parseExp();
        const val = evalSimple(valAst);

        rho[name] = val;
        gamma[name] = inferTypeVal(val);

        const next = parser.peek();
        if (next.type === TokenType.SEMI || next.type === TokenType.COMMA) {
            parser.consume();
        } else if (next.type !== TokenType.EOF) {
        }
    }

    return { rho, gamma };
}

function ensureParsed() {
    if (appState.parsed) return true;
    const input = document.getElementById("inputExpr").value;
    try {
        let initRho = {},
            initGamma = {};
        try {
            const envData = parseEnvInput();
            initRho = envData.rho;
            initGamma = envData.gamma;
        } catch (e) {
            throw new Error("Env Error: " + e.message);
        }

        const tokens = tokenize(input);
        const parser = new Parser(tokens);
        const ast = parser.parse();

        try {
            appState.typeTree = deriveType(initGamma, ast);
            assignIds(appState.typeTree, { val: 0 });
        } catch (e) {
            appState.typeTree = { error: e.message };
        }

        try {
            appState.opTree = deriveOp(initRho, ast);
            assignIds(appState.opTree, { val: 0 });
        } catch (e) {
            appState.opTree = { error: e.message };
        }

        appState.parsed = true;
        return true;
    } catch (e) {
        document.getElementById("typeOutput").innerHTML =
            `<div class="error">${e.message}</div>`;
        document.getElementById("opOutput").innerHTML =
            `<div class="error">${e.message}</div>`;
        return false;
    }
}

function deriveAll() {
    if (!ensureParsed()) return;
    appState.step = 99999;
    refreshView();
}

function step() {
    if (!ensureParsed()) return;
    appState.step++;
    refreshView();
}

function reset() {
    resetState();
}

function fmtVal(v) {
    if (Array.isArray(v)) return `(${v.map(fmtVal).join(", ")})`;
    return v;
}

// NEW: Formats env based on the 'shorten' toggle
function formatEnv(env, type, shorten) {
    const keys = Object.keys(env);
    const symbol = type === "type" ? "θ" : "ρ";

    // Always use symbol if empty or if shorten is requested
    if (keys.length === 0 || shorten) return symbol;

    // Otherwise, show full details
    const entries = keys.map((k) => {
        const val = env[k];
        // Use existing fmtVal for operational values, raw string for types
        const valStr = type === "op" ? fmtVal(val) : val;
        const sep = type === "type" ? ":" : "=";
        return `${k}${sep}${valStr}`;
    });
    return `{${entries.join(", ")}}`;
}

function render(node, type, shorten, maxStep) {
    if (node.error)
        return (document.createElement("div").innerHTML =
            `<div class="error">${node.error}</div>`);

    if (node.id >= maxStep) return null;

    const wrapper = document.createElement("div");
    wrapper.className = "node-wrapper";

    const ruleDiv = document.createElement("div");
    ruleDiv.className = `inference-rule ${type}-rule`;

    const premDiv = document.createElement("div");
    premDiv.className = "premises";

    node.children.forEach((c) => {
        const childNode = render(c, type, shorten, maxStep);
        if (childNode) premDiv.appendChild(childNode);
    });
    ruleDiv.appendChild(premDiv);

    const conDiv = document.createElement("div");
    conDiv.className = "conclusion-container";

    let eStr = "";
    if (node.expr.type === "Custom") eStr = node.expr.str;
    else eStr = exprToString(node.expr, shorten);

    // UPDATED: Generate environment string dynamically
    const envDisplay = formatEnv(node.env, type, shorten);

    const relation = type === "type" ? ":" : "⇒";
    const txt = `${envDisplay} ⊢ ${eStr} ${relation} ${fmtVal(node.res)}`;

    conDiv.innerHTML = `<span class="conclusion">${txt}</span>`;
    ruleDiv.appendChild(conDiv);

    const labelDiv = document.createElement("div");
    labelDiv.className = "rule-label";
    labelDiv.innerText = node.rule;

    wrapper.appendChild(ruleDiv);
    wrapper.appendChild(labelDiv);
    return wrapper;
}

function refreshView() {
    const shorten = document.getElementById("shortenToggle").checked;
    const tOut = document.getElementById("typeOutput");
    const opOut = document.getElementById("opOutput");

    tOut.innerHTML = "";
    opOut.innerHTML = "";

    if (appState.typeTree) {
        if (appState.typeTree.error)
            tOut.innerHTML = `<div class="error">${appState.typeTree.error}</div>`;
        else {
            const dom = render(appState.typeTree, "type", shorten, appState.step);
            if (dom) tOut.appendChild(dom);
            else
                tOut.innerHTML = '<div style="color:#888">Click Step to start...</div>';
        }
    }

    if (appState.opTree) {
        if (appState.opTree.error)
            opOut.innerHTML = `<div class="error">${appState.opTree.error}</div>`;
        else {
            const dom = render(appState.opTree, "op", shorten, appState.step);
            if (dom) opOut.appendChild(dom);
            else
                opOut.innerHTML =
                    '<div style="color:#888">Click Step to start...</div>';
        }
    }

    document.getElementById("typeCounter").innerText =
        appState.typeTree && !appState.typeTree.error
            ? `(Step ${Math.min(appState.step, appState.typeTree.id + 10)})`
            : "";
    document.getElementById("opCounter").innerText =
        appState.opTree && !appState.opTree.error
            ? `(Step ${Math.min(appState.step, appState.opTree.id + 10)})`
            : "";
}

/** * ==================================================================
 * 6. RULES VISUALIZATION & TOOLTIP LOGIC
 * ==================================================================
 */

// --- DATA: Type Rules [cite: 95-133] ---
const TYPE_RULES_DB = {
    "T-INTLIT": {
        premises: ["i ∈ IntLit"],
        conclusion: "θ ⊢ i : Int",
    },
    "T-BOOLLIT": {
        premises: ["b ∈ BoolLit"],
        conclusion: "θ ⊢ b : Boolean",
    },
    "T-IDENT": {
        premises: ["θ(x) = τ"],
        conclusion: "θ ⊢ x : τ",
    },
    "T-INTPLUS": {
        premises: ["θ ⊢ e₁ : Int", "θ ⊢ e₂ : Int"],
        conclusion: "θ ⊢ e₁ + e₂ : Int",
    },
    "T-INTNEG": {
        premises: ["θ ⊢ e : Int"],
        conclusion: "θ ⊢ -e : Int",
    },
    "T-NOT": {
        premises: ["θ ⊢ e : Boolean"],
        conclusion: "θ ⊢ !e : Boolean",
    },
    "T-COMP": {
        premises: ["θ ⊢ e₁ : Int", "θ ⊢ e₂ : Int"],
        conclusion: "θ ⊢ e₁ < e₂ : Boolean",
    },
    "T-EQ": {
        premises: ["θ ⊢ e₁ : τ", "θ ⊢ e₂ : τ"],
        conclusion: "θ ⊢ e₁ == e₂ : Boolean",
    },
    "T-IF": {
        premises: ["θ ⊢ e₁ : Boolean", "θ ⊢ e₂ : τ", "θ ⊢ e₃ : τ"],
        conclusion: "θ ⊢ if (e₁) e₂ else e₃ : τ",
    },
    "T-BLOCK": {
        premises: ["θ ⊢ d : θ'", "θ' ⊢ e : τ"],
        conclusion: "θ ⊢ { d; e } : τ",
    },
    "T-VALDECL": {
        premises: ["θ ⊢ e : τ", "θ' = θ[x : τ]"],
        conclusion: "θ ⊢ val x = e : θ'",
    },
    "T-TUPLE": {
        premises: ["θ ⊢ e₁ : τ₁ ... θ ⊢ eₙ : τₙ"],
        conclusion: "θ ⊢ (e₁...eₙ) : (τ₁...τₙ)",
    },
    "T-MATCH": {
        premises: ["θ ⊢ e : (τ₁..τₙ)", "θ' = θ[x₁:τ₁..]", "θ' ⊢ e₂ : τ"],
        conclusion: "θ ⊢ e match { case (x₁..) => e₂ } : τ",
    },
};

// --- DATA: Op Rules [cite: 31-94] ---
const OP_RULES_DB = {
    INTLIT: { premises: [], conclusion: "ρ ⊢ i ⇒ i" },
    TRUE: { premises: [], conclusion: "ρ ⊢ true ⇒ true" },
    FALSE: { premises: [], conclusion: "ρ ⊢ false ⇒ false" },
    IDENT: { premises: ["ρ(x) = v"], conclusion: "ρ ⊢ x ⇒ v" },
    INTPLUS: {
        premises: ["ρ ⊢ e₁ ⇒ v₁", "ρ ⊢ e₂ ⇒ v₂", "v = v₁ + v₂"],
        conclusion: "ρ ⊢ e₁ + e₂ ⇒ v",
    },
    INTSUB: {
        premises: ["ρ ⊢ e₁ ⇒ v₁", "ρ ⊢ e₂ ⇒ v₂", "v = v₁ - v₂"],
        conclusion: "ρ ⊢ e₁ - e₂ ⇒ v",
    },
    INTMUL: {
        premises: ["ρ ⊢ e₁ ⇒ v₁", "ρ ⊢ e₂ ⇒ v₂", "v = v₁ * v₂"],
        conclusion: "ρ ⊢ e₁ * e₂ ⇒ v",
    },
    INTDIV: {
        premises: ["ρ ⊢ e₁ ⇒ v₁", "ρ ⊢ e₂ ⇒ v₂", "v = v₁ / v₂"],
        conclusion: "ρ ⊢ e₁ / e₂ ⇒ v",
    },
    INTNEG: { premises: ["ρ ⊢ e ⇒ v", "v' = -v"], conclusion: "ρ ⊢ -e ⇒ v'" },
    EQTRUE: {
        premises: ["ρ ⊢ e₁ ⇒ v₁", "ρ ⊢ e₂ ⇒ v₂", "v₁ = v₂"],
        conclusion: "ρ ⊢ e₁ == e₂ ⇒ true",
    },
    EQFALSE: {
        premises: ["ρ ⊢ e₁ ⇒ v₁", "ρ ⊢ e₂ ⇒ v₂", "v₁ ≠ v₂"],
        conclusion: "ρ ⊢ e₁ == e₂ ⇒ false",
    },
    LETRUE: {
        premises: ["ρ ⊢ e₁ ⇒ v₁", "ρ ⊢ e₂ ⇒ v₂", "v₁ ≤ v₂"],
        conclusion: "ρ ⊢ e₁ <= e₂ ⇒ true",
    }, // [cite: 66, 67]
    LEFALSE: {
        premises: ["ρ ⊢ e₁ ⇒ v₁", "ρ ⊢ e₂ ⇒ v₂", "v₁ > v₂"],
        conclusion: "ρ ⊢ e₁ <= e₂ ⇒ false",
    },
    IFTRUE: {
        premises: ["ρ ⊢ e₁ ⇒ true", "ρ ⊢ e₂ ⇒ v"],
        conclusion: "ρ ⊢ if (e₁) e₂ else e₃ ⇒ v",
    },
    IFFALSE: {
        premises: ["ρ ⊢ e₁ ⇒ false", "ρ ⊢ e₃ ⇒ v"],
        conclusion: "ρ ⊢ if (e₁) e₂ else e₃ ⇒ v",
    },
    BLOCK: {
        premises: ["ρ ⊢ d ⇒ ρ'", "ρ' ⊢ e ⇒ v"],
        conclusion: "ρ ⊢ { d; e } ⇒ v",
    },
    VALDECL: {
        premises: ["ρ ⊢ e ⇒ v", "ρ' = ρ[x ↦ v]"],
        conclusion: "ρ ⊢ val x = e ⇒ ρ'",
    },
    TUPLE: {
        premises: ["ρ ⊢ e₁ ⇒ v₁ ... ρ ⊢ eₙ ⇒ vₙ"],
        conclusion: "ρ ⊢ (e₁...eₙ) ⇒ (v₁...vₙ)",
    },
    MATCH: {
        premises: ["ρ ⊢ e ⇒ (v₁..vₙ)", "ρ' = ρ[x₁↦v₁..]", "ρ' ⊢ e₂ ⇒ v"],
        conclusion: "ρ ⊢ e match { case (x₁..) => e₂ } ⇒ v",
    },
};

// Helper: Generate HTML card
function generateRuleHTML(name, ruleData, idPrefix) {
    if (!ruleData)
        return `<div class="rule-card"><b>${name}</b><span>(No def)</span></div>`;

    const premisesHTML =
        ruleData.premises.length > 0
            ? ruleData.premises.map((p) => `<span>${p}</span>`).join("")
            : "&nbsp;";

    // Add ID for scroll targeting
    return `
    <div class="rule-card" id="${idPrefix}-${name}">
      <div class="rule-title">${name}</div>
      <div class="math-rule">
        <div class="math-premises">${premisesHTML}</div>
        <div class="math-conclusion">${ruleData.conclusion}</div>
      </div>
    </div>
  `;
}

// 1. Initialize Grids
function initRulesGrid() {
    const opGrid = document.getElementById("opRulesGrid");
    const typeGrid = document.getElementById("typeRulesGrid");

    opGrid.innerHTML = "";
    typeGrid.innerHTML = "";

    Object.keys(OP_RULES_DB).forEach((key) => {
        const wrapper = document.createElement("div");
        wrapper.innerHTML = generateRuleHTML(key, OP_RULES_DB[key], "rule-ref-op");
        opGrid.appendChild(wrapper.firstElementChild);
    });

    Object.keys(TYPE_RULES_DB).forEach((key) => {
        const wrapper = document.createElement("div");
        wrapper.innerHTML = generateRuleHTML(
            key,
            TYPE_RULES_DB[key],
            "rule-ref-type",
        );
        typeGrid.appendChild(wrapper.firstElementChild);
    });
}

// 2. Full Screen Toggle
function toggleFullScreen(panelId) {
    const panel = document.getElementById(panelId);
    const isFull = panel.classList.contains("fullscreen-panel");

    // Close any open fullscreen first
    document.querySelectorAll(".fullscreen-panel").forEach((p) => {
        p.classList.remove("fullscreen-panel");
        p.querySelector(".btn-expand").innerText = "⛶";
    });

    if (!isFull) {
        panel.classList.add("fullscreen-panel");
        panel.querySelector(".btn-expand").innerText = "❌"; // Close icon
    }
}

// 3. Scroll to Rule Logic
function scrollToRule(ruleName, type) {
    // Normalize rule name variants (e.g., LTTRUE -> LETRUE for lookup if needed)
    let db = type === "type" ? TYPE_RULES_DB : OP_RULES_DB;
    let targetKey = ruleName;

    // Fallback logic for Op rules (grouping GT/LT/LE under similar visuals if DB missing entries)
    if (type === "op") {
        if (ruleName.includes("LT") || ruleName.includes("GT"))
            targetKey = "LETRUE"; // Visual fallback
        if (ruleName.includes("EQ")) targetKey = "EQTRUE";
    }
    // Check if exists in DB
    if (!db[targetKey]) {
        // If exact match fails, try generic lookup (e.g. remove "TRUE/FALSE")
        const base = Object.keys(db).find((k) => ruleName.startsWith(k));
        if (base) targetKey = base;
    }

    const idPrefix = type === "type" ? "rule-ref-type" : "rule-ref-op";
    const element = document.getElementById(`${idPrefix}-${targetKey}`);

    if (element) {
        element.scrollIntoView({ behavior: "smooth", block: "center" });
        element.classList.add("highlight-rule");
        setTimeout(() => element.classList.remove("highlight-rule"), 2000);
    } else {
        console.warn("Rule definition not found for:", ruleName);
    }
}

// 4. Global Event Listeners (Tooltip & Click)
function initInteractions() {
    const tooltip = document.getElementById("ruleTooltip");

    // Unified handler for both trees
    ["typeOutput", "opOutput"].forEach((containerId) => {
        const container = document.getElementById(containerId);
        if (!container) return;

        const isType = containerId === "typeOutput";
        const db = isType ? TYPE_RULES_DB : OP_RULES_DB;

        // Hover (Tooltip)
        container.addEventListener("mouseover", (e) => {
            if (e.target.classList.contains("rule-label")) {
                const ruleName = e.target.innerText;
                let data = db[ruleName];

                // Simple fallback for tooltips
                if (!data && !isType && ruleName.includes("LT")) data = db["LETRUE"];

                if (data) {
                    tooltip.innerHTML = generateRuleHTML(ruleName, data, "tooltip");
                    tooltip.style.display = "flex";
                }
            }
        });

        // Move
        container.addEventListener("mousemove", (e) => {
            if (tooltip.style.display === "flex") {
                tooltip.style.left = e.clientX + 15 + "px";
                tooltip.style.top = e.clientY + 15 + "px";
            }
        });

        // Leave
        container.addEventListener("mouseout", () => {
            tooltip.style.display = "none";
        });

        // Click (Scroll)
        container.addEventListener("click", (e) => {
            if (e.target.classList.contains("rule-label")) {
                scrollToRule(e.target.innerText, isType ? "type" : "op");
            }
        });
    });
}

// Initialize on Load
window.addEventListener("DOMContentLoaded", () => {
    initRulesGrid();
    initInteractions();
});
