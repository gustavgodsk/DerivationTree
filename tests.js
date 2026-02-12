/** * ==================================================================
 * 5. AUTOMATED TEST SUITE
 * ==================================================================
 */

const testCases = [
    // --- Basic Arithmetic (IntOps) ---
    {
        name: "INTLIT",
        env: "",
        expr: "42",
        expected: 42,
    },
    {
        name: "INTNEG (Unary Minus)",
        env: "",
        expr: "-5",
        expected: -5,
    },
    {
        name: "INTPLUS",
        env: "",
        expr: "10 + 32",
        expected: 42,
    },
    {
        name: "INTSUB",
        env: "",
        expr: "10 - 20",
        expected: -10,
    },
    {
        name: "INTMUL",
        env: "",
        expr: "5 * 5",
        expected: 25,
    },
    {
        name: "INTDIV (Integer Division)",
        env: "",
        expr: "10 / 3",
        expected: 3, // Truncated
    },
    {
        name: "INTMOD",
        env: "",
        expr: "10 % 3",
        expected: 1,
    },

    // --- Boolean Logic ---
    {
        name: "TRUE/FALSE Lit",
        env: "",
        expr: "true",
        expected: true,
    },
    {
        name: "NOT (UnOp)",
        env: "",
        expr: "!true",
        expected: false,
    },
    {
        name: "EQTRUE (Integers)",
        env: "",
        expr: "5 == 5",
        expected: true,
    },
    {
        name: "EQFALSE (Integers)",
        env: "",
        expr: "5 == 6",
        expected: false,
    },
    {
        name: "LTTRUE",
        env: "",
        expr: "5 < 10",
        expected: true,
    },
    {
        name: "LTFALSE",
        env: "",
        expr: "10 < 5",
        expected: false,
    },
    {
        name: "GTTRUE",
        env: "",
        expr: "10 > 5",
        expected: true,
    },
    {
        name: "GTFALSE",
        env: "",
        expr: "5 > 10",
        expected: false,
    },
    // The fixed <= operator
    {
        name: "LETRUE (Equal)",
        env: "",
        expr: "5 <= 5",
        expected: true,
    },
    {
        name: "LETRUE (Less)",
        env: "",
        expr: "4 <= 5",
        expected: true,
    },
    {
        name: "LEFALSE",
        env: "",
        expr: "6 <= 5",
        expected: false,
    },

    // --- Variables & Environments ---
    {
        name: "IDENT (Lookup)",
        env: "x = 100",
        expr: "x",
        expected: 100,
    },
    {
        name: "IDENT (Shadowing in Block)",
        env: "x = 10",
        expr: "{ val x = 20; x }",
        expected: 20,
    },

    // --- Control Flow ---
    {
        name: "IFTRUE",
        env: "",
        expr: "if (true) 1 else 2",
        expected: 1,
    },
    {
        name: "IFFALSE",
        env: "",
        expr: "if (false) 1 else 2",
        expected: 2,
    },
    {
        name: "IF (Complex Condition)",
        env: "x=5",
        expr: "if (x < 10) x * 2 else x",
        expected: 10,
    },

    // --- Blocks & Declarations ---
    {
        name: "BLOCK (Sequence)",
        env: "",
        expr: "{ val x = 1; { val y = 2; x + y } }",
        expected: 3,
    },
    {
        name: "BLOCK (Scope Check)",
        env: "y=10",
        expr: "{ val x = 5; y + x }",
        expected: 15,
    },

    // --- Tuples ---
    {
        name: "TUPLE (Creation)",
        env: "",
        expr: "(1, 2)",
        expected: [1, 2],
    },
    {
        name: "TUPLE (Nested)",
        env: "",
        expr: "(1, (2, 3))",
        expected: [1, [2, 3]],
    },

    // --- Match (Pattern Matching) ---
    {
        name: "MATCH (Basic)",
        env: "t = (10, 20)",
        expr: "t match { case (a, b) => a + b }",
        expected: 30,
    },
    {
        name: "MATCH (Shadowing)",
        env: "x = 100",
        expr: "(1, 2) match { case (x, y) => x }",
        expected: 1,
    },
    {
        name: "MATCH (Nested Eval)",
        env: "",
        expr: "({val x=1; x}, 2) match { case (a, b) => a + b }",
        expected: 3,
    },
];

function runTests() {
    console.log("--- Starting Test Suite ---");
    let passed = 0;
    let failed = 0;

    testCases.forEach((test) => {
        try {
            // 1. Setup Environment
            let initRho = {};
            if (test.env) {
                const envData = parseEnvString(test.env); // Helper function defined below
                initRho = envData;
            }

            // 2. Parse Expression
            const tokens = tokenize(test.expr);
            const parser = new Parser(tokens);
            const ast = parser.parse();

            // 3. Run Operational Semantics
            const resultTree = deriveOp(initRho, ast);
            const actual = resultTree.res;

            // 4. Compare
            if (isEqual(actual, test.expected)) {
                console.log(`%c[PASS] ${test.name}`, "color: green");
                passed++;
            } else {
                console.error(`[FAIL] ${test.name}`);
                console.log(`   Expr: ${test.expr}`);
                console.log(`   Expected: ${JSON.stringify(test.expected)}`);
                console.log(`   Actual:   ${JSON.stringify(actual)}`);
                failed++;
            }
        } catch (e) {
            console.error(`[ERROR] ${test.name} crashed: ${e.message}`);
            failed++;
        }
    });

    const summaryColor = failed === 0 ? "green" : "red";
    const summaryText = `Tests Completed: ${passed}/${testCases.length} Passed.`;

    console.log(
        `%c${summaryText}`,
        `color: ${summaryColor}; font-weight: bold; font-size: 14px;`,
    );
}

// Helper to compare values (handles nested arrays for tuples)
function isEqual(a, b) {
    if (Array.isArray(a) && Array.isArray(b)) {
        if (a.length !== b.length) return false;
        for (let i = 0; i < a.length; i++) {
            if (!isEqual(a[i], b[i])) return false;
        }
        return true;
    }
    return a === b;
}

// Helper to parse env string specifically for tests (simplified version of main parser)
function parseEnvString(str) {
    const rho = {};
    if (!str) return rho;

    // Simple split by semicolon for test cases
    const pairs = str.split(";");
    pairs.forEach((pair) => {
        const [k, v] = pair.split("=");
        if (k && v) {
            const key = k.trim();
            const valStr = v.trim();
            // Basic parsing for integers and tuples
            if (valStr.startsWith("(")) {
                // Remove parens and split
                const nums = valStr
                    .slice(1, -1)
                    .split(",")
                    .map((n) => parseInt(n.trim()));
                rho[key] = nums;
            } else {
                rho[key] = parseInt(valStr);
            }
        }
    });
    return rho;
}
runTests();
