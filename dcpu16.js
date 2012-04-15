
function DCPU16() {

var kRegNames = [ 'A', 'B', 'C', 'X', 'Y', 'Z', 'I', 'J'];
var kOpNames = [
    'xxx', 'SET', 'ADD', 'SUB', 'MUL', 'DIV', 'MOD', 'SHL',
    'SHR', 'AND', 'BOR', 'XOR', 'IFE', 'IFN', 'IFG', 'IFB',
];

function getRegisterIdx(regname) {
    regname = regname.toUpperCase();

    for (var i = 0; i < kRegNames.length; ++i) {
        if (kRegNames[i] === regname)
            return i;
    }

    return -1;
}

function getSimpleOpIdx(opname) {
    // NB: ignore kOpNames[0]

    for (var i = 1; i < kOpNames.length; ++i) {
        if (kOpNames[i] === opname)
            return i;
    }

    return -1;
}

function getUnescapedChar(c) {
    switch(c) {
        case 'n':   return '\n';
        case 'r':   return "\r";
        case 't':   return "\t";
        case 'v':   return "\v";
        case '\\':  return "\\";
        case '\/':  return "/";
    }

    return -1;
}

function isNumber(n) {
    return !isNaN(parseInt(n)) && isFinite(n);
}

function operandHasData(operand) {
    return (operand >= 0x10 && operand < 0x18) || operand == 0x1e || operand == 0x1f;
}

function opLength(op, a, b) {
    var len = 1;

    if (op == 0) {
        if (a == 0x1) {
            // jsr - skip address
            ++len;
        }
    } else {
        if (operandHasData(a))
            ++len;
        if (operandHasData(b))
            ++len;
    }                   

    return len;     
}

function toBinaryString(v, bits) {
    if (!bits) bits = 0;

    var t = v.toString(2);
    while  (t.length < bits)
        t = '0' + t;
    return t;
}

function printIt(s) {
    $("#output").append("<div>" + s + "</div>");
}


var dcpu = {
 
makeDisassembler : function(_code) {
    var disassembler = {
        code : _code,
        PC : 0,

        operandName : function(operand, operand_data) {

            if (operand >= 0x0 && operand < 0x8) {
                return kRegNames[operand];
            } else if (operand < 0x10) {
                return '[' + kRegNames[operand - 0x8] + ']';
            } else if (operand < 0x18) {
                var next_word = this.code[operand_data];
                return '[0x' + next_word.toString(16) + ' + ' + kRegNames[operand - 0x10] + ']';

            } else if (operand < 0x20) {
                switch(operand)
                {
                    case 0x18: return '[SP++]';
                    case 0x19: return '[SP]';
                    case 0x1a: return '[--SP]';
                    case 0x1b: return 'SP';
                    case 0x1c: return 'PC';
                    case 0x1d: return 'O';
                    case 0x1e: return '[0x' + this.code[operand_data].toString(16) + ']';
                    case 0x1f: return  '0x' + this.code[operand_data].toString(16);
                }
            } else if (operand < 0x40) {
                return '0x' + (operand-0x20).toString(16);
            }
            return '???';
        },

        disasm : function() {
            var opcode = this.code[this.PC++];
            var op = (opcode&0x000f);
            var a  = (opcode&0x03f0)>>4;
            var b  = (opcode&0xfc00)>>10;

            if (op == 0) {
                if (a == 0x1) {
                    var bdat = this.PC; if (operandHasData(b)) ++this.PC;
                    return 'JSR ' + this.operandName(b, bdat);
                }

                return '???';
            } else if (op < kOpNames.length) {

                var adat = this.PC; if (operandHasData(a)) ++this.PC;
                var bdat = this.PC; if (operandHasData(b)) ++this.PC;

                return kOpNames[op] + ' ' + this.operandName(a, adat) + ', ' + this.operandName(b, bdat);
            }

            return '???';
        }
    };

    return disassembler;
},

makeAssembler : function() {
    var kUpperBoundLo       = "A".charCodeAt(0);
    var kUpperBoundHi       = "Z".charCodeAt(0);
    var kLowerBoundLo       = "a".charCodeAt(0);
    var kLowerBoundHi       = "z".charCodeAt(0);
    var kNumberBoundLo      = "0".charCodeAt(0);
    var kNumberBoundHi      = "9".charCodeAt(0);
    var kUnderscore         = "_".charCodeAt(0);
    var kComma              = ",".charCodeAt(0);
    var kColon              = ":".charCodeAt(0);
    var kSemiColon          = ";".charCodeAt(0);
    var kSpace              = " ".charCodeAt(0);
    var kTab                = "\t".charCodeAt(0);
    var kDoubleQuote        = "\"".charCodeAt(0);
    var kBackslash          = "\\".charCodeAt(0);
    var kHash               = "#".charCodeAt(0);
    var kLeftSquareBracket  = "[".charCodeAt(0);
    var kRightSquareBracket = "]".charCodeAt(0);
    var kLeftCurlyBrace     = "{".charCodeAt(0);
    var kRightCurlyBrace    = "}".charCodeAt(0);
    var kLeftParens         = "(".charCodeAt(0);
    var kRightParens        = ")".charCodeAt(0);

    function isLabelChar(c) {
        return (c >= kUpperBoundLo  && c <= kUpperBoundHi) ||
               (c >= kLowerBoundLo  && c <= kLowerBoundHi) ||
               (c >= kNumberBoundLo && c <= kNumberBoundHi) ||
               c == kUnderscore;
    }

    function isOpcodeChar(c) {
        return (c >= kUpperBoundLo  && c <= kUpperBoundHi) ||
               (c >= kLowerBoundLo  && c <= kLowerBoundHi);
    }   

    function isWhitespace(c) {
        return c == kSpace || c == kTab;
    }

    // Return list of operand and data values. If data value resolves to a string, it's a label which needs to be mapped onto an address.
    function packInstruction(instruction, labels) {

        var opname = instruction.op.text.toUpperCase();

        var op_idx = getSimpleOpIdx(opname);

        var r = [];

        if (op_idx > 0) {
            if (instruction.operands.length != 2)
                throw {name:'ParseError', message:'Expecting 2 operands', sourceLoc:instruction.sourceLoc};

            var opa = instruction.operands[0];
            var opb = instruction.operands[1];

            var a = packOperand(opa, labels);
            var b = packOperand(opb, labels);

            r.push( (b<<10) | (a<<4) | op_idx );
            if (opa.hasOwnProperty('data'))
                r.push(opa.data);
            if (opb.hasOwnProperty('data'))
                r.push(opb.data);               
            return r;
        } else {
            if (opname === 'JSR') {
                if (instruction.operands.length != 1)
                    throw {name:'ParseError', message:'Expecting 1 operand', sourceLoc:instruction.sourceLoc};

                var operand = instruction.operands[0];
                var loc = packOperand(operand, labels);

                var subop = 0x1;        // JSR
                r.push( (loc<<10) | (subop<<4) | 0x00 );
                if (operand.hasOwnProperty('data'))
                    r.push(operand.data);
                return r;
            } else if (opname === 'DAT') {

                for (var i = 0; i < instruction.operands.length; ++i) {
                    var operand = instruction.operands[i];
                    if (operand.hasOwnProperty('b')) {
                        throw {name:'ParseError', message:'Expecting simple operand', sourceLoc:operand.sourceLoc};
                    }

                    if (operand.is_string_literal) {
                        for (var c = 0; c < operand.value.length; ++c) {
                            r.push( operand.value.charCodeAt(c) );
                        }
                    } else if (operand.is_numeric_literal) {
                        if (operand.value > 0xffff)
                            throw {name:'ParseError', message:'Data value is too large', sourceLoc:operand.sourceLoc};

                        r.push( operand.value );
                    } else {
                        throw {name:'ParseError', message:'Expecting a literal', sourceLoc:operand.sourceLoc};
                    }
                }
                return r;
            } else if (opname === 'RESERVE') {
                if (instruction.operands.length != 1)
                    throw {name:'ParseError', message:'Expecting 1 operand', sourceLoc:instruction.sourceLoc};

                var operand = instruction.operands[0];

                if (operand.is_numeric_literal) {
                    var val = operand.value;
                    if (val > 0xffff)
                        throw {name:'ParseError', message:'Argument for RESERVE is too large', sourceLoc:operand.sourceLoc};
                    for (var i = 0; i < val; ++i)
                        r.push( 0 );

                    return r;
                } else {
                    throw {name:'ParseError', message:'Expecting a numeric literal', sourceLoc:operand.sourceLoc};
                }
            }
        }

        throw {name:'ParseError', message:"Unknown opcode '" + instruction.op.text + "'", sourceLoc:instruction.op.sourceLoc};
    }

    function packOperand(operand, labels) {

        if (operand.is_memory_access) {
            var a    = operand.value.a;
            var rega = getRegisterIdx(a);

            if (operand.value.hasOwnProperty('b')) {
                var b    = operand.value.b;
                var regb = getRegisterIdx(b);
                var val;
                var reg;
                var is_label = 0;
                if (rega >= 0 && regb >= 0) {
                    throw {name:'ParseError', message:"No operand which takes two registers", sourceLoc:operand.sourceLoc};
                } else if (rega >= 0) {
                    reg = rega;
                    if (isNumber(b)) {
                        val = parseInt(b);
                    } else if (labels.hasOwnProperty(b)) {
                        val = b;
                        is_label = 1;
                    } else {
                        throw {name:'ParseError', message:"Unknown operand form", sourceLoc:operand.sourceLoc};
                    }
                } else if (regb >= 0) {
                    reg = regb;
                    if (isNumber(a)) {
                        val = parseInt(a);
                    } else if (labels.hasOwnProperty(a)) {
                        val = a;
                        is_label = 1;
                    } else {
                        throw {name:'ParseError', message:"No operand which takes two literals", sourceLoc:operand.sourceLoc};                  
                    }
                } else {
                    throw {name:'ParseError', message:"Unknown operand form", sourceLoc:operand.sourceLoc};
                }

                if (is_label || val > 0) {
                    operand.data = val;     // next_word
                    return 0x10 + reg;
                } else {
                    return 0x08 + reg;
                }
            } else {
                if (rega >= 0) {
                    return 0x08 + rega;
                } else if (isNumber(a)) {
                    operand.data = parseInt(a); // next_word
                    return 0x1e;
                } else if(labels.hasOwnProperty(a)) {
                    operand.data = a;   // next_word
                    return 0x1e;
                } else {
                    throw {name:'ParseError', message:'Unhandled memory access form', sourceLoc:operand.sourceLoc};
                }
            }

        } else if (operand.is_identifier) {
            var id  = operand.value;
            var reg = getRegisterIdx(id);
            if (reg >= 0) {
                return 0x00 + reg;
            } else if (id === 'POP' || id === 'pop') {
                return 0x18;
            } else if (id === 'PEEK' || id === 'peek') {
                return 0x19;
            } else if (id === 'PUSH' || id === 'push') {
                return 0x1a;
            } else if (id === 'SP' || id === 'sp') {
                return 0x1b;
            } else if (id === 'PC' || id === 'pc') {
                return 0x1c;
            } else if (id === 'O' || id === 'o') {
                return 0x1d;
            } else {
                if (labels.hasOwnProperty(id)) {
                    // Store the label in .data - we remap these to numeric values later
                    operand.data = id; // next_word
                    return 0x1f;
                } else {
                    throw {name:'ParseError', message:"Unknown label '" + id + "'", sourceLoc:operand.sourceLoc};
                }
            }
        } else if (operand.is_numeric_literal) {
            var val = operand.value;
            if (val > 0xffff) {
                throw {name:'ParseError', message:"Numeric literal is too large", sourceLoc:operand.sourceLoc};
            } else if (val >= 0x20) {
                operand.data = val; // next_word
                return 0x1f;
            } else {
                return 0x20 + val;
            }
        } else if (operand.is_string_literal) {
            throw {name:'ParseError', message:"Unexpected string literal", sourceLoc:operand.sourceLoc};
        }

        throw {name:'ParseError', message:'Unhandled case', sourceLoc:operand.sourceLoc};
    }

    function makeLexer(line, sourceLoc) {

        var i = 0;
        var end = line.length;

        var lexer = {

            popChar : function() {
                return i < end ? line.charCodeAt(i++) : -1;
            },

            peekChar : function() {
                return i < end ? line.charCodeAt(i) : -1;
            },

            empty : function() {
                return i >= end;
            },

            getLabel : function() {
                var b = i;
                while(i < end && isLabelChar(line.charCodeAt(i)))
                    ++i;

                return i > b ? line.substr(b, i-b) : null;
            },

            getOpCode : function() {
                this.skipWhite();

                var b = i;
                while(i < end && isOpcodeChar(line.charCodeAt(i)))
                    ++i;

                if (i <= b)
                    return null

                return {text:line.substr(b, i-b), sourceLoc:{line:sourceLoc.line, col:b, colStart:b, colEnd:i}};
            },

            getOperand : function() {
                if(i >= end)
                    return null;

                this.skipWhite();

                var operand = {};

                operand.sourceLoc = {line:sourceLoc.line, col:i, colStart:i, colEnd:i};

                if (line.charCodeAt(i) == kLeftSquareBracket) {

                    operand.is_memory_access = 1;

                    ++i;    // Left bracket
                    var b = i;
                    while(i < end && line.charCodeAt(i) != kRightSquareBracket)
                        ++i;                        

                    if (i >= end)
                        throw {name:'ParseError', message:"Missing ']'", sourceLoc:operand.sourceLoc};

                    var text = line.substr(b, i-b);

                    operand.value = {};

                    // Look for an operand of the form REG+Literal or Literal+REG (avoid matchi on '[SP++])
                    var plusidx = text.indexOf('+');
                    if (plusidx >= 0 && text.indexOf('+', plusidx+1) < 0) {
                        operand.value.a = $.trim( text.substring(0, plusidx) );
                        operand.value.b = $.trim( text.substring(plusidx+1) );
                    } else {
                        operand.value.a = $.trim( text );
                    }

                    ++i;    // Right bracket

                    // FIXME: if this is [SP++], [SP], [--SP] need switch for 'PUSH', 'PEEK' etc

                } else if (line.charCodeAt(i) == kDoubleQuote) {

                    operand.is_string_literal = 1;

                    ++i;    // quote
                    var str = '';
                    var in_escape = 0;
                    var matched_closing_quote = 0;
                    while(i < end) {
                        var char_code = line.charCodeAt(i);
                        var char      = line.charAt(i);
                        if (in_escape) {
                            var unescaped = getUnescapedChar( char );
                            if (unescaped < 0) {
                                throw {name:'ParseError', message:"Unknown escape sequence: '" + char + "'", sourceLoc:{line:operand.sourceLoc.line, col:i, colStart:i, colEnd:i}};
                            }
                            str = str + unescaped;
                            in_escape = 0;
                        } else if (char_code == kDoubleQuote) {
                            ++i;
                            matched_closing_quote = 1;
                            break;
                        } else if (char_code == kBackslash) {
                            in_escape = 1;
                        } else {
                            str = str + char;
                        }
                        ++i;
                    }

                    if (!matched_closing_quote)
                        throw {name:'ParseError', message:"Missing '\"'", sourceLoc:operand.sourceLoc};

                    operand.value = str;
                } else {
                    var b = i;
                    while(i < end) {
                        var c = line.charCodeAt(i);
                        if (c == kComma || c == kSemiColon)
                            break;
                        ++i;
                    }

                    var text = line.substr(b, i-b);
                    text = $.trim(text);

                    if (isNumber(text)) {
                        operand.is_numeric_literal = 1;
                        operand.value = parseInt(text);
                    } else {
                        operand.is_identifier = 1;
                        operand.value = text;
                    }
                }

                // update the sourceLoc to reflect end
                // FIXME: this overestimates the end of the second arg: includes whitespace
                operand.sourceLoc.colEnd = i;

                return operand;
            },


            skipWhite : function() {
                while(i < end && isWhitespace(line.charCodeAt(i)))
                    ++i;
            },
        };

        return lexer;
    }

    var assembler = {

        instructions : [],
        labels : {},            // map of labelname -> instruction idx
        code : null,

        errorText : '',

        displayParseError : function(lines, e) {
            var err = '';
            if (e.name==='ParseError') {

                if (e.sourceLoc) {
                    var indent = '  ';
                    err += "Error line " + (e.sourceLoc.line+1) + ":" + (e.sourceLoc.col+1) + " : " + e.message + "\n";

                    var line = lines[e.sourceLoc.line];

                    err += indent + line + "\n";

                    if (e.sourceLoc.hasOwnProperty('colStart') && e.sourceLoc.hasOwnProperty('colEnd')) {
                        var i = 0;
                        var t = '';
                        for ( ; i < e.sourceLoc.colStart; ++i )
                            t += line.charCodeAt(i) == kTab ? '\t' : ' ';
                        t += '^';
                        ++i;
                        for ( ; i+1 < e.sourceLoc.colEnd; ++i )
                            t += '~';
                        if (i > e.sourceLoc.colStart && i < e.sourceLoc.colEnd)
                            t += '^';

                        err += indent + t + "\n";
                    }

                } else {
                    err += "Error: " + e.message + "\n";
                }
            } else {
                err += "Unhandled exception: " + e.name + ", " + e.message + "\n";
            }

            this.errorText += err;
        },

        preprocess : function(lines) {

            var pplines = [];       // array of {sourceLoc, lineText}

            var curLine = 0;
            while (curLine < lines.length) {
                var line = lines[curLine];
                var ppline = {sourceLoc:{line:curLine,col:0}, lineText:line};
                ++curLine;

                if (line.length == 0)
                    continue;

                if (line.charCodeAt(0) == kHash) {
                    // macro

                    var macro = [];
                    while (curLine < lines.length) {
                        line = lines[curLine];
                        ppline = {sourceLoc:{line:curLine,col:0}, lineText:line};

                        if (line.length > 0 && line.charCodeAt(0) == kRightCurlyBrace) {
                            ++curLine;
                            break;
                        }

                        macro.push(ppline);
                        ++curLine;
                    }

                    // store macros somewhere...

                } else {
                    pplines.push(ppline);
                }

            }

            return pplines;
        },

        assemble : function(text) {

            var lines = text.split('\n');

            // preprocess, extract all macros, strip comments etc.
            var pplines = this.preprocess(lines);

            for (var i = 0; i < pplines.length; ++i) {
                var ppline = pplines[i];

                try {
                    this.parseLine(ppline.lineText, ppline.sourceLoc);
                } catch (e) {
                    this.displayParseError(lines, e);
                }
            }

            try {
                this.finalise(lines);
            } catch (e) {
                this.displayParseError(lines, e);
            }

            if (this.errorText.length > 0) {
                printIt( '<pre>' + this.errorText + '</pre>' );
            }           
        },

        parseLine : function(line, sourceLoc) {

            var lexer = makeLexer(line, sourceLoc);

            // Ignore blank lines (or lines just containing a comment)
            lexer.skipWhite();
            if (lexer.empty())
                return;

            // Check for a label here
            var first_char = lexer.peekChar();
            if (first_char === kColon) {
                lexer.popChar();

                var label = lexer.getLabel();
                if (!label)
                    throw {name:'ParseError', message:'Expecting a label', sourceLoc:sourceLoc};

                this.labels[label] = this.instructions.length;
            }

            // Bail out if the rest of the line is empty, or only contains a semicolon
            lexer.skipWhite();
            if (lexer.empty() || lexer.peekChar() === kSemiColon)
                return;

            // Check for opcode
            var opcode = lexer.getOpCode();
            if (opcode) {

                lexer.skipWhite();
                if (lexer.peekChar() == kLeftParens) {
                    throw {name:'ParseError', message:'Macros not supported', sourceLoc:sourceLoc};
                }

                var instruction = {op: opcode, operands: [], sourceLoc:sourceLoc};
                this.instructions.push(instruction);

                while (1) {
                    var operand = lexer.getOperand();
                    if (!operand)
                        break;

                    instruction.operands.push(operand);
    
                    lexer.skipWhite();
                    if (lexer.peekChar() != kComma)
                        break;
                    lexer.popChar();
                }

                // pseudo ops
                if (opcode.text.toUpperCase() == 'JMP') {
                    if (instruction.operands.length != 1)
                        throw {name:'ParseError', message:'Expecting 1 operand', sourceLoc:instruction.sourceLoc};

                    instruction.op = {text:'SET', sourceLoc:instruction.op.sourceLoc};
                    instruction.operands = [ {a:'PC', is_memory_access:0, sourceLoc:instruction.sourceLoc}, instruction.operands[0] ];
                }

            } else {
                throw {name:'ParseError', message:'Expecting a label or opcode', sourceLoc:sourceLoc};
            }
        },

        finalise : function(lines) {

            var buf = [];
            for (var i = 0; i < this.instructions.length; ++i) {
                var instruction = this.instructions[i];

                try {
                    var r = packInstruction(instruction, this.labels);

                    instruction.address = buf.length;
                    buf = buf.concat( r );

                } catch(e) {
                    this.displayParseError(lines, e);
                }
            }

            // second pass to remap label data values
            for (var i = 0; i < buf.length; ++i) {
                if ((typeof buf[i]) === 'string') {
                    var label = buf[i];
                    if (this.labels.hasOwnProperty(label)) {
                            var instruction_idx = this.labels[label];
                        buf[i] = this.instructions[instruction_idx].address;
                    } else {
                        throw {name:'ParseError', message:"Undefined label '" + label + '"'};
                    }
                }
            }

            this.code = new Uint16Array(buf.length);
            for (var i = 0; i < buf.length; ++i)
                this.code[i] = buf[i];

        },
    };

    return assembler;
},
 
makePuter : function() {

    var puter = {
        code : null,
        data : new Uint16Array(0x10000),
        regs : new Uint16Array(8),      // A B C X Y Z I J
        PC : 0,
        SP : 0,
        O : 0,
        CondExec : 1,

        loadCode :  function(code) {
            this.code = code;   // Keep track of the original code. 

            if (code) {
                for (var i = 0; i < code.length; ++i) {
                    this.data[i] = code[i];
                };
            }
            for (var i = 0; i < this.regs.length; ++i) {
                this.regs[i] = 0;
            }

            this.PC = 0;
            this.SP = 0;
            this.O = 0;
            this.CondExec = 1;
        },

        jsr : function(addr) {
            this.data[ --this.SP ] = this.PC;
            this.PC = addr;
        },

        loadOperand : function(operand) {
            if (operand >= 0x0 && operand < 0x8) {
                return this.regs[operand];
            } else if (operand < 0x10) {
                var r = this.regs[operand - 0x8];
                return this.data[r];
            } else if (operand < 0x18) {
                var next_word = this.data[this.PC++];
                var r = this.regs[operand - 0x10];
                return this.data[next_word + r];
            } else if (operand < 0x20) {
                switch(operand)
                {
                    case 0x18: return this.data[  this.SP++];
                    case 0x19: return this.data[  this.SP  ];
                    case 0x1a: return this.data[--this.SP  ];
                    case 0x1b: return this.SP;
                    case 0x1c: return this.PC;
                    case 0x1d: return this.O;
                    case 0x1e: return this.data[this.data[this.PC++]];
                    case 0x1f: return this.data[this.PC++];
                }
            } else if (operand < 0x40) {
                return operand - 0x20;
            }

            throw 'fail - crazy operand for loadOperand: ' + operand;
        },

        storeResult : function(operand, value, next_word_addr, orig_sp) {
            if (operand >= 0x0 && operand < 0x8) {
                this.regs[operand] = value;
                return;
            } else if (operand < 0x10) {
                var r = this.regs[operand - 0x8];
                this.data[r] = value;
                return;
            } else if (operand < 0x18) {
                var next_word = this.data[next_word_addr];
                var r         = this.regs[operand - 0x10];
                this.data[next_word + r] = value;
                return;
            } else if (operand < 0x20) {
                switch(operand)
                {
                    case 0x18: this.data[ orig_sp   ] = value; return;
                    case 0x19: this.data[ orig_sp   ] = value; return;
                    case 0x1a: this.data[ orig_sp-1 ] = value; return;
                    case 0x1b: this.SP = value; return;
                    case 0x1c: this.PC = value; return;
                    case 0x1d: this.O  = value; return;
                    case 0x1e: this.data[this.data[next_word_addr]] = value; return;
                    case 0x1f: /* can't assign to literal - silently fail */ return;
                }
            } else if (operand < 0x40) {
                // can't assign to literal - silently fail
                return;
            }

            throw 'fail - crazy operand for store: ' + operand;         
        },

        run : function(cycle_count) {

            for( var count = 0; count < cycle_count; ++count ) {
                var orig_sp = this.SP;

                var opcode = this.data[this.PC++];
                var op = (opcode&0x000f);
                var a  = (opcode&0x03f0)>>4;
                var b  = (opcode&0xfc00)>>10;

                //printIt((this.CondExec ? "&nbsp;" : "-") +  "PC " + (this.PC-1) + ": " + opcode.toString(16));

                if (this.CondExec) {
    
                    if (op == 0)
                    {
                        switch(a) {
                            case 0x00: 
                                throw 'reserved';
                            case 0x01:
                                this.jsr(this.loadOperand(b));
                                break;
                            default:
                                throw 'reserved';
                        }

                    } else {
                        var a_next_word = this.PC;  // keep track of this, for writeback
                        var aval = this.loadOperand(a); 
                        var bval = this.loadOperand(b);

                        var rval;
                        var o = this.O;

                        switch(op)
                        {
                            case 0x1:
                                rval = bval;
                                break;
                            case 0x2:
                                rval = aval + bval;
                                o = rval > 0xffff ? 0x1 : 0x0;
                                break;
                            case 0x3:
                                rval = aval - bval;
                                o = rval < 0 ? 0xffff : 0x0;
                                break;
                            case 0x4:
                                rval = aval * bval;
                                o = ((aval*bval)>>16)&0xffff;
                                break;
                            case 0x5:
                                rval = bval ? aval/bval : 0;
                                o = bval ? ((aval<<16)/bval)&0xffff : 0;
                                break;
                            case 0x6:
                                rval = bval ? aval%bval : 0;
                                break;
                            case 0x7:
                                rval = aval << bval;
                                o = ((aval<<bval)>>16)&0xffff;
                                break;
                            case 0x8:
                                rval = aval >> bval;
                                o = ((aval<<16)>>b)&0xffff;
                                break;
                            case 0x9: rval = aval & bval;               break;
                            case 0xa: rval = aval | bval;               break;
                            case 0xb: rval = aval ^ bval;               break;
                            case 0xc: this.CondExec = aval == bval;     break;
                            case 0xd: this.CondExec = aval != bval;     break;
                            case 0xe: this.CondExec = aval > bval;      break;
                            case 0xf: this.CondExec = (aval&bval) != 0; break;
                            default:
                                throw 'fail - crazy opcode';
                        }

                        if (op < 0xc) {
                            this.O = o;
                            this.storeResult(a, rval, a_next_word, orig_sp);
                        }
                    }

                } else {
                    // Predicated instruction - skipped
                    this.CondExec = 1;
                    this.PC += opLength(op, a, b)-1; // -1 as PC has already been incremented
                }
            }
        },
    };

    return puter;
},

displayDisassembly : function(code, cur_pc) {

    var $pre = $('<table />');

    if (code) {
        var dis = this.makeDisassembler(code);
        $pre.append('<tr><th width=100></th><th></th><th></th></tr>');

        while (dis.PC < dis.code.length) {
            var pc = dis.PC;
            var d = dis.disasm();

            var ops = '';
            for (var i = pc; i < dis.PC; ++i) {
                var op = dis.code[i].toString(16);
                while(op.length < 4) op = '0' + op;
                ops +=  op + ' ';
            }

            var $tr = $('<tr><td>0x' + pc.toString(16) + '</td><td>' + ops + '</td><td>' + d + '</td></tr>');
            if (pc === cur_pc)
                $tr.attr('bgcolor', '#eeeeee');
            $pre.append($tr);
        }   
    }

    $('#disasm').html($pre);
},

loadFontData : function(puter) {
    var fnt = $('#font')[0];

    var $canvas = $( '<canvas />', {width:fnt.width, height:fnt.height} );
    if ($canvas[0].getContext){
        var ctx = $canvas[0].getContext('2d');

        // blit font into temp canvas
        ctx.drawImage(fnt, 0, 0);

        var img_data   = ctx.getImageData(0, 0, fnt.width, fnt.height);
        var font_bytes = img_data.data;

        var pixel_stride = 4;
        var row_stride = img_data.width*pixel_stride;

        for (var glyph = 0; glyph < 32*4; ++glyph)
        {
            var col = glyph % 32;
            var row = Math.floor(glyph / 32);

            var src_x = col * 4;
            var src_y = row * 8;

            var row_idx = (src_y*row_stride) + (src_x*pixel_stride);

            var columns = [0,0,0,0];

            for (var y = 0; y < 8; ++y)
            {
                var bit = 0x80 >> y;

                var idx = row_idx;
                for (var x = 0; x < 4; ++x)
                {
                    var d0 = font_bytes[idx+0] + font_bytes[idx+1] + font_bytes[idx+2];
                    if (d0 > 384)
                    {
                        columns[x] |= bit;
                    }

                    idx += pixel_stride;
                }

                row_idx += row_stride;
            }

            var ab = (columns[0]<<8) | columns[1];
            var cd = (columns[2]<<8) | columns[3];

            puter.data[0x8180 + glyph*2 + 0] = ab;
            puter.data[0x8180 + glyph*2 + 1] = cd;
        }
    }
},

displayState : function(puter) {
    var $table = $('<table class="table table-condensed register-table" />');

    $table.append('<tr><th>Register</th><th>hex</th><th>dec</th><th>bin</th></tr>')
    $table.append('<tr><td>PC</td><td>0x' + puter.PC.toString(16) + '</td><td>' + puter.PC.toString(10) + '</td><td>' + toBinaryString(puter.PC, 16) + '</td></tr>' );
    $table.append('<tr><td>SP</td><td>0x' + puter.SP.toString(16) + '</td><td>' + puter.SP.toString(10) + '</td><td>' + toBinaryString(puter.SP, 16) + '</td></tr>' );
    $table.append('<tr><td>O</td><td>0x' + puter.O.toString(16) + '</td><td>' + puter.O.toString(10) + '</td><td>' + toBinaryString(puter.O, 16) + '</td></tr>' );
    for (var i  = 0; i < 8; ++i ) {
        var val = puter.regs[i];
        $table.append('<tr><td>' + kRegNames[i] + '</td><td>0x' + val.toString(16) + '</td><td>' + val.toString(10) + '</td><td>' + toBinaryString(val, 16) + '</td></tr>');
    }

    $('#registers').html($table);

    var kNumScreenChars = 32*12;
    if (!puter.lastScreen) {
        puter.lastScreen = new Uint16Array(kNumScreenChars);
        for (var i = 0; i < kNumScreenChars; ++i)
            puter.lastScreen[i] = -1;
    }

    // http://0x10cwiki.com/wiki/Video_RAM
    var colours = [
        '#000000',  // 0
        '#0000aa',  // 1
        '#00aa00',  // 2
        '#00aaaa',  // 3
        '#aa0000',  // 4
        '#aa00aa',  // 5
        '#aa5500',  // 6
        '#aaaaaa',  // 7
        '#555555',  // 8
        '#5555ff',  // 9
        '#55ff55',  // a
        '#55ffff',  // b
        '#ff55ff',  // c
        '#ff55ff',  // d
        '#ffff55',  // e
        '#ffffff',  // f
    ];


    var canvas = document.getElementById('display');
    if (canvas.getContext) {
        var screen_ctx = canvas.getContext('2d');

        var scale = 4;

        for(var chidx = 0; chidx < kNumScreenChars; ++chidx) {
            var column = chidx % 32;
            var row    = Math.floor(chidx/32)

            var ch = puter.data[0x8000 + chidx];

            // Skip render if this character hasn't changed
            if (ch == puter.lastScreen[chidx]) {
                ++chidx;
                continue;
            }
            puter.lastScreen[chidx] = ch;

            var fg    = (ch>>12)&0xf;
            var bg    = (ch>> 8)&0xf;
            var blink = (ch>> 7)&0x1;
            var glyph = (ch    )&0x7f;

            var glyph_ab = puter.data[0x8180 + glyph*2 + 0];
            var glyph_cd = puter.data[0x8180 + glyph*2 + 1];

            var cols = [ (glyph_ab>>8)&0xff,
                         (glyph_ab   )&0xff,
                         (glyph_cd>>8)&0xff,
                         (glyph_cd   )&0xff ];

            var display_x = (column*4) * scale;
            var display_y = (row*8)    * scale;

            screen_ctx.fillStyle = colours[bg];
            screen_ctx.fillRect(display_x, display_y, 4*scale, 8*scale);

            for (var y = 0; y < 8; ++y) {
                var bit = 0x80 >> y;

                for (var x = 0; x < 4; ++x) {

                    if (cols[x]&bit) {
                        screen_ctx.fillStyle = colours[fg];
                        screen_ctx.fillRect(display_x + x*scale, display_y + y*scale, scale, scale);
                    }
                }
            }

        }

        //screen_ctx.putImageData(img_data, 0, 0);


    }

},

parseSource : function(text) {

    var assembler = this.makeAssembler();
    assembler.assemble(text);
    return assembler.code;
},

parseBinary : function(text) {

  var lines = text.split('\n');
  var data = [];

  for (var i = 0; i < lines.length; ++i) {
    var line = lines[i];

    // Strip anything up to the first colon (this is colum for address)
    var c = line.indexOf(':');
    if (c >= 0)
      line = line.substr(c+2);

    var vals = line.split(' ');
    for (var v = 0; v < vals.length; ++v) {
      if (vals[v]) {
        data.push( parseInt(vals[v], 16) );
      }
    }
  }

  var buf = new Uint16Array(data.length);
  for (var i = 0; i < data.length; ++i)
    buf[i] = data[i];

  return buf;
},

};

return dcpu;

} // dcpu16
