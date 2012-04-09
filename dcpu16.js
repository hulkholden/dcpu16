
function DCPU16() {

var kRegNames = [ 'A', 'B', 'C', 'X', 'Y', 'Z', 'I', 'J'];
var kOpNames = [
	'xxx', 'SET', 'ADD', 'SUB', 'MUL', 'DIV', 'MOD', 'SHL',
	'SHR', 'AND', 'BOR', 'XOR', 'IFE', 'IFN', 'IFG', 'IFB',
];

function getRegisterIdx(regname) {
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

				return 'xxx';
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
	var kLeftSquareBracket  = "[".charCodeAt(0);
	var kRightSquareBracket = "]".charCodeAt(0);

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

		var op_idx = getSimpleOpIdx(instruction.op);

		var r = [];

		if (op_idx > 0) {
			if (instruction.operands.length != 2)
				throw {name:'ParseError', message:'Expecting 2 operands'};

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
			if (instruction.op === 'JSR') {
				if (instruction.operands.length != 1)
					throw {name:'ParseError', message:'Expecting 1 operand'};

				var opa = instruction.operands[0];
				var b = packOperand(opa, labels);

				var a = 0x1;		// JSR
				r.push( (b<<10) | (a<<4) | 0x00 );
				if (opa.hasOwnProperty('data'))
					r.push(opa.data);
				return r;
			}
		}

		throw {name:'ParseError', message:"Unknown opcode '" + instruction.op + "'"};
	}

	function packOperand(operand, labels) {

		var a = operand.a;

		if (operand.hasOwnProperty('b')) {
			var b = operand.b;

			var rega = getRegisterIdx(a);
			var regb = getRegisterIdx(b);
			var val;
			var reg;
			if (rega >= 0 && regb >= 0) {
				throw {name:'ParseError', message:"No operand which takes two registers"};
			} else if (rega >= 0 && isNumber(b)) {
				reg = rega;
				val = parseInt(b);
			} else if (regb >= 0 && isNumber(a)) {
				reg = regb;
				val = parseInt(a);
			} else {
				throw {name:'ParseError', message:"No operand which takes two literals"};
			}

			if (val > 0) {
				operand.data = val;		// next_word
				return 0x10 + reg;
			} else {
				return 0x08 + reg;
			}
		} else {
			var reg = getRegisterIdx(a);
			if (reg >= 0) {
				return reg + (operand.is_memory_access ? 0x08 : 0x00);
			} else if (a === 'POP' || a === 'pop' || a === '[SP++]') {
				return 0x18;
			} else if (a === 'PEEK' || a === 'peek' || a === '[SP]') {
				return 0x19;
			} else if (a === 'PUSH' || a === 'push' || a === '[--SP') {
				return 0x1a;
			} else if (a === 'SP' || a === 'sp') {
				return 0x1b;
			} else if (a === 'PC' || a === 'pc') {
				return 0x1c;
			} else if (a === 'O' || a === 'o') {
				return 0x1d;
			} else {
				var val;
				var is_label = 0;
				if (isNumber(a)) {
					val = parseInt(a);
				} else {
					if (labels.hasOwnProperty(a)) {
						is_label = 1;
						val = a;		// address filled in later, set to label string now
					} else {
						throw {name:'ParseError', message:"Unknown label '" + a + "'"};
					}
				}
				if (operand.is_memory_access) {
					operand.data = val;	// next_word
					return 0x1e;
				} else if (is_label || val >= 0x20) {
					operand.data = val; // next_word
					return 0x1f;
				} else {
					return 0x20 + val;
				}	
			}
		}

		throw {name:'ParseError', message:'Unhandled case'};
	}

	function makeLexer(line) {

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

				return i > b ? line.substr(b, i-b) : null;
			},

			getOperand : function() {
				if(i >= end)
					return null;

				this.skipWhite();

				var text = '';
				var is_memory_access = 0;

				if (line.charCodeAt(i) == kLeftSquareBracket) {

					is_memory_access = 1;

					++i;	// Left bracket
					var b = i;
					while(i < end && line.charCodeAt(i) != kRightSquareBracket)
						++i;						

					if (i >= end)
						throw {name:'ParseError', message:"Missing ']'"};

					text = line.substr(b, i-b);

					++i;	// Right bracket

				} else {
					var b = i;
					while(i < end) {
						var c = line.charCodeAt(i);
						if (c == kComma || c == kSemiColon)
							break;
						++i;
					}
					text = line.substr(b, i-b);
				}

				var operand = {'is_memory_access':is_memory_access};

				// Look for an operand of the form REG+Literal or Literal+REG (avoid matchi on '[SP++])
				var plusidx = text.indexOf('+');
				if (plusidx >= 0 && text.indexOf('+', plusidx+1) < 0) {

					if (!is_memory_access) {
						throw {name:'ParseError', message:'This form is only valid for memory access: [REG+literal]'};
					}

					operand.a = $.trim( text.substring(0, plusidx) );
					operand.b = $.trim( text.substring(plusidx+1) );
				} else {
					operand.a = $.trim( text );
				}

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
		labels : {},			// map of labelname -> instruction idx

		parseLine : function(line) {

			var lexer = makeLexer(line);

			if (lexer.empty())
				return;

			// Check for a label here
			if (lexer.peekChar() == kColon) {
				lexer.popChar();

				var label = lexer.getLabel();
				if (!label)
					throw {name:'ParseError', message:'Expecting a label'};

				this.labels[label] = this.instructions.length;
			}

			// Check for opcode
			var opcode = lexer.getOpCode();
			if (opcode) {
				var instruction = {op: opcode, operands: []};
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
			}
		},

		finalise : function() {

			var buf = [];
			for (var i = 0; i < this.instructions.length; ++i) {
				var instruction = this.instructions[i];
				var r = packInstruction(instruction, this.labels);

				instruction.address = buf.length;
				buf = buf.concat( r );
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

		  	var code = new Uint16Array(buf.length);
		  	for (var i = 0; i < buf.length; ++i)
		  		code[i] = buf[i];

			return code;
		},
	};

	return assembler;
},
 
makePuter : function() {

	var puter = {
		data : new Uint16Array(0x10000),
		regs : new Uint16Array(8),		// A B C X Y Z I J
		PC : 0,
		SP : 0xffff,
		O : 0,
		CondExec : 1,

		loadCode :  function(code) {
			for (var i = 0; i < code.length; ++i) {
				this.data[i] = code[i];
			};
			for (var i = 0; i < this.regs.length; ++i) {
				this.regs[i] = 0;
			}

			this.PC = 0;
			this.SP = 0xffff;
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
						var a_next_word = this.PC;	// keep track of this, for writeback
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
							case 0x9: rval = aval & bval;				break;
							case 0xa: rval = aval | bval;				break;
							case 0xb: rval = aval ^ bval;				break;
							case 0xc: this.CondExec = aval == bval; 	break;
							case 0xd: this.CondExec = aval != bval;		break;
							case 0xe: this.CondExec = aval > bval;  	break;
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
	var dis = this.makeDisassembler(code);
	var $pre = $('<table />');

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


	$('#disasm').html($pre);
},

displayState : function(puter) {
	var $table = $('<table class="table table-condensed" style="table-layout:fixed" />');

	var $row = $('<tr />');
	$row.append('<th>PC</th>');
	$row.append('<th>SP</th>');
	$row.append('<th>O</th>');
	for (var i  = 0; i < 8; ++i ) {
		$row.append('<th>' + kRegNames[i] + '</th>');
	}

	var $thead = $('<thead />');
	$thead.append($row);
	$table.append($thead);

	$row = $('<tr />');
	$row.append('<td>0x' + puter.PC.toString(16) + '</td>');
	$row.append('<td>0x' + puter.SP.toString(16) + '</td>');
	$row.append('<td>0x' + puter.O.toString(16)  + '</td>');
	for (var i  = 0; i < 8; ++i ) {
		$row.append('<td>0x' + puter.regs[i].toString(16) + '</td>');
	}

	$table.append($row);
	$('#registers').html($table);
},

parseSource : function(text) {

	var assembler = this.makeAssembler();

	var lines = text.split('\n');
	for (var i = 0; i < lines.length; ++i) {
		var line = lines[i];
		assembler.parseLine(line);
	}
	return assembler.finalise();
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

