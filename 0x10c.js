
var gProgram = [
        0x7c01, 0x0030, 0x7de1, 0x1000, 0x0020, 0x7803, 0x1000, 0xc00d,
        0x7dc1, 0x001a, 0xa861, 0x7c01, 0x2000, 0x2161, 0x2000, 0x8463,
        0x806d, 0x7dc1, 0x000d, 0x9031, 0x7c10, 0x0018, 0x7dc1, 0x001a,
        0x9037, 0x61c1, 0x7dc1, 0x001a, 
 ];

// Bail when PC hits this
var gHaltOnPC = 26;

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

 
function makeDisassembler(_code) {
	var disassembler = {
		code : _code,
		regnames : [ 'A', 'B', 'C', 'X', 'Y', 'Z', 'I', 'J'],
		ops : [
			'xxx', 'SET', 'ADD', 'SUB', 'MUL', 'DIV', 'MOD', 'SHL',
			'SHR', 'AND', 'BOR', 'XOR', 'IFE', 'IFN', 'IFG', 'IFB',
		],
		PC : 0,

		operandName : function(operand, operand_data) {

			if (operand >= 0x0 && operand < 0x8) {
				return this.regnames[operand];
			} else if (operand < 0x10) {
				return '[' + this.regnames[operand - 0x8] + ']';
			} else if (operand < 0x18) {
				var next_word = this.code[operand_data];
				return '[0x' + next_word.toString(16) + ' + ' + this.regnames[operand - 0x10] + ']';

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
					return 'JSR ' + this.operandName(b, this.PC);
				}

				return 'xxx';
			} else if (op < this.ops.length) {

				var adat = this.PC; if (operandHasData(a)) ++this.PC;
				var bdat = this.PC; if (operandHasData(b)) ++this.PC;

				return this.ops[op] + ' ' + this.operandName(a, adat) + ', ' + this.operandName(b, bdat);
			}

			return '???';
		}
	};

	return disassembler;
} 

function makePuter(code) {

	var stream = {
		data : new Uint16Array(0x10000),
		regs : new Uint16Array(8),		// A B C X Y Z I J
		PC : 0,
		SP : 0xffff,
		O : 0,
		CondExec : 1,

		regnames : [ 'A', 'B', 'C', 'X', 'Y', 'Z', 'I', 'J'],

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
				if (this.PC == gHaltOnPC)
					break;

				var orig_sp = this.SP;

				var opcode = this.data[this.PC++];
				var op = (opcode&0x000f);
				var a  = (opcode&0x03f0)>>4;
				var b  = (opcode&0xfc00)>>10;

				printIt((this.CondExec ? "&nbsp;" : "-") +  "PC " + (this.PC-1) + ": " + opcode.toString(16));

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

	stream.loadCode(code);

	return stream;
}

function displayDisassembly(code) {
	var dis = makeDisassembler(code);
	var $pre = $('<pre/>');

	while (dis.PC < dis.code.length) {
		var d = dis.disasm() + '\n';
		$pre.append(d);
	}
	$('#disasm').append($pre);

}

function displayState(puter) {
	var $table = $('<table class="table table-condensed" style="table-layout:fixed" />');

	var $row = $('<tr />');
	$row.append('<th>PC</th>');
	$row.append('<th>SP</th>');
	$row.append('<th>O</th>');
	for (var i  = 0; i < 8; ++i ) {
		$row.append('<th>' + puter.regnames[i] + '</th>');
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
}

function execute(name, data) {

	var c = '<div class="row"><div class="span12"><div id="disasm"></div><div id="registers"></div><div id="output"></div></div></div>';
	$('#container').append(c);

	var puter = makePuter(data);
	printIt('<h2>Running ' + name + '</h2>');

	displayDisassembly(data);


	try
	{
		puter.run(10000);

		displayState(puter);

	}
	catch(e)
	{
		printIt( 'Exception: ' + e );
	}

	printIt("Halted");
}


function printIt(s) {
	$("#output").append("<div>" + s + "</div>");
}

execute('test', gProgram);
