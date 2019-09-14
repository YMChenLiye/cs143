
//**************************************************************
//
// Code generator SKELETON
//
// Read the comments carefully. Make sure to
//    initialize the base class tags in
//       `CgenClassTable::CgenClassTable'
//
//    Add the label for the dispatch tables to
//       `IntEntry::code_def'
//       `StringEntry::code_def'
//       `BoolConst::code_def'
//
//    Add code to emit everyting else that is needed
//       in `CgenClassTable::code'
//
//
// The files as provided will produce code to begin the code
// segments, declare globals, and emit constants.  You must
// fill in the rest.
//
//**************************************************************

#include "cgen.h"
#include <algorithm>
#include <set>
#include "cgen_gc.h"

extern void emit_string_constant(ostream& str, char* s);
extern int cgen_debug;

//
// Three symbols from the semantic analyzer (semant.cc) are used.
// If e : No_type, then no code is generated for e.
// Special code is generated for new SELF_TYPE.
// The name "self" also generates code different from other references.
//
//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
Symbol arg, arg2, Bool, concat, cool_abort, copy, Int, in_int, in_string, IO, length, Main, main_meth, No_class,
    No_type, Object, out_int, out_string, prim_slot, self, SELF_TYPE, Str, str_field, substr, type_name, val;
//
// Initializing the predefined symbols.
//
static void initialize_constants(void)
{
    arg = idtable.add_string("arg");
    arg2 = idtable.add_string("arg2");
    Bool = idtable.add_string("Bool");
    concat = idtable.add_string("concat");
    cool_abort = idtable.add_string("abort");
    copy = idtable.add_string("copy");
    Int = idtable.add_string("Int");
    in_int = idtable.add_string("in_int");
    in_string = idtable.add_string("in_string");
    IO = idtable.add_string("IO");
    length = idtable.add_string("length");
    Main = idtable.add_string("Main");
    main_meth = idtable.add_string("main");
    //   _no_class is a symbol that can't be the name of any
    //   user-defined class.
    No_class = idtable.add_string("_no_class");
    No_type = idtable.add_string("_no_type");
    Object = idtable.add_string("Object");
    out_int = idtable.add_string("out_int");
    out_string = idtable.add_string("out_string");
    prim_slot = idtable.add_string("_prim_slot");
    self = idtable.add_string("self");
    SELF_TYPE = idtable.add_string("SELF_TYPE");
    Str = idtable.add_string("String");
    str_field = idtable.add_string("_str_field");
    substr = idtable.add_string("substr");
    type_name = idtable.add_string("type_name");
    val = idtable.add_string("_val");
}

static char* gc_init_names[] = {"_NoGC_Init", "_GenGC_Init", "_ScnGC_Init"};
static char* gc_collect_names[] = {"_NoGC_Collect", "_GenGC_Collect", "_ScnGC_Collect"};

//  BoolConst is a class that implements code generation for operations
//  on the two booleans, which are given global names here.
BoolConst falsebool(FALSE);
BoolConst truebool(TRUE);

//*********************************************************
//
// Define method for code generation
//
// This is the method called by the compiler driver
// `cgtest.cc'. cgen takes an `ostream' to which the assembly will be
// emmitted, and it passes this and the class list of the
// code generator tree to the constructor for `CgenClassTable'.
// That constructor performs all of the work of the code
// generator.
//
//*********************************************************

void program_class::cgen(ostream& os)
{
    // spim wants comments to start with '#'
    os << "# start of generated code\n";

    initialize_constants();
    CgenClassTable* codegen_classtable = new CgenClassTable(classes, os);

    os << "\n# end of generated code\n";
}

//////////////////////////////////////////////////////////////////////////////
//
//  emit_* procedures
//
//  emit_X  writes code for operation "X" to the output stream.
//  There is an emit_X for each opcode X, as well as emit_ functions
//  for generating names according to the naming conventions (see emit.h)
//  and calls to support functions defined in the trap handler.
//
//  Register names and addresses are passed as strings.  See `emit.h'
//  for symbolic names you can use to refer to the strings.
//
//////////////////////////////////////////////////////////////////////////////

static void emit_load(char* dest_reg, int offset, char* source_reg, ostream& s)
{
    s << LW << dest_reg << " " << offset * WORD_SIZE << "(" << source_reg << ")" << endl;
}

static void emit_store(char* source_reg, int offset, char* dest_reg, ostream& s)
{
    s << SW << source_reg << " " << offset * WORD_SIZE << "(" << dest_reg << ")" << endl;
}

static void emit_load_imm(char* dest_reg, int val, ostream& s)
{
    s << LI << dest_reg << " " << val << endl;
}

static void emit_load_address(char* dest_reg, char* address, ostream& s)
{
    s << LA << dest_reg << " " << address << endl;
}

static void emit_partial_load_address(char* dest_reg, ostream& s)
{
    s << LA << dest_reg << " ";
}

static void emit_load_bool(char* dest, const BoolConst& b, ostream& s)
{
    emit_partial_load_address(dest, s);
    b.code_ref(s);
    s << endl;
}

static void emit_load_string(char* dest, StringEntry* str, ostream& s)
{
    emit_partial_load_address(dest, s);
    str->code_ref(s);
    s << endl;
}

static void emit_load_int(char* dest, IntEntry* i, ostream& s)
{
    emit_partial_load_address(dest, s);
    i->code_ref(s);
    s << endl;
}

static void emit_move(char* dest_reg, char* source_reg, ostream& s)
{
    s << MOVE << dest_reg << " " << source_reg << endl;
}

static void emit_neg(char* dest, char* src1, ostream& s)
{
    s << NEG << dest << " " << src1 << endl;
}

static void emit_add(char* dest, char* src1, char* src2, ostream& s)
{
    s << ADD << dest << " " << src1 << " " << src2 << endl;
}

static void emit_addu(char* dest, char* src1, char* src2, ostream& s)
{
    s << ADDU << dest << " " << src1 << " " << src2 << endl;
}

static void emit_addiu(char* dest, char* src1, int imm, ostream& s)
{
    s << ADDIU << dest << " " << src1 << " " << imm << endl;
}

static void emit_div(char* dest, char* src1, char* src2, ostream& s)
{
    s << DIV << dest << " " << src1 << " " << src2 << endl;
}

static void emit_mul(char* dest, char* src1, char* src2, ostream& s)
{
    s << MUL << dest << " " << src1 << " " << src2 << endl;
}

static void emit_sub(char* dest, char* src1, char* src2, ostream& s)
{
    s << SUB << dest << " " << src1 << " " << src2 << endl;
}

static void emit_sll(char* dest, char* src1, int num, ostream& s)
{
    s << SLL << dest << " " << src1 << " " << num << endl;
}

static void emit_jalr(char* dest, ostream& s)
{
    s << JALR << "\t" << dest << endl;
}

static void emit_jal(char* address, ostream& s)
{
    s << JAL << address << endl;
}

static void emit_return(ostream& s)
{
    s << RET << endl;
}

static void emit_gc_assign(ostream& s)
{
    s << JAL << "_GenGC_Assign" << endl;
}

static void emit_disptable_ref(Symbol sym, ostream& s)
{
    s << sym << DISPTAB_SUFFIX;
}

static void emit_init_ref(Symbol sym, ostream& s)
{
    s << sym << CLASSINIT_SUFFIX;
}

static void emit_label_ref(int l, ostream& s)
{
    s << "label" << l;
}

static void emit_protobj_ref(Symbol sym, ostream& s)
{
    s << sym << PROTOBJ_SUFFIX;
}

static void emit_method_ref(Symbol classname, Symbol methodname, ostream& s)
{
    s << classname << METHOD_SEP << methodname;
}

static void emit_label_def(int l, ostream& s)
{
    emit_label_ref(l, s);
    s << ":" << endl;
}

static void emit_beqz(char* source, int label, ostream& s)
{
    s << BEQZ << source << " ";
    emit_label_ref(label, s);
    s << endl;
}

static void emit_beq(char* src1, char* src2, int label, ostream& s)
{
    s << BEQ << src1 << " " << src2 << " ";
    emit_label_ref(label, s);
    s << endl;
}

static void emit_bne(char* src1, char* src2, int label, ostream& s)
{
    s << BNE << src1 << " " << src2 << " ";
    emit_label_ref(label, s);
    s << endl;
}

static void emit_bleq(char* src1, char* src2, int label, ostream& s)
{
    s << BLEQ << src1 << " " << src2 << " ";
    emit_label_ref(label, s);
    s << endl;
}

static void emit_blt(char* src1, char* src2, int label, ostream& s)
{
    s << BLT << src1 << " " << src2 << " ";
    emit_label_ref(label, s);
    s << endl;
}

static void emit_blti(char* src1, int imm, int label, ostream& s)
{
    s << BLT << src1 << " " << imm << " ";
    emit_label_ref(label, s);
    s << endl;
}

static void emit_bgti(char* src1, int imm, int label, ostream& s)
{
    s << BGT << src1 << " " << imm << " ";
    emit_label_ref(label, s);
    s << endl;
}

static void emit_branch(int l, ostream& s)
{
    s << BRANCH;
    emit_label_ref(l, s);
    s << endl;
}

//
// Push a register on the stack. The stack grows towards smaller addresses.
//
static void emit_push(char* reg, ostream& str)
{
    emit_store(reg, 0, SP, str);
    emit_addiu(SP, SP, -4, str);
}

static void emit_pop(char* reg, ostream& str)
{
    emit_load(reg, 1, SP, str);
    emit_addiu(SP, SP, 4, str);
}

//
// Fetch the integer value in an Int object.
// Emits code to fetch the integer value of the Integer object pointed
// to by register source into the register dest
//
static void emit_fetch_int(char* dest, char* source, ostream& s)
{
    emit_load(dest, DEFAULT_OBJFIELDS, source, s);
}

//
// Emits code to store the integer value contained in register source
// into the Integer object pointed to by dest.
//
static void emit_store_int(char* source, char* dest, ostream& s)
{
    emit_store(source, DEFAULT_OBJFIELDS, dest, s);
}

static void emit_test_collector(ostream& s)
{
    emit_push(ACC, s);
    emit_move(ACC, SP, s);   // stack end
    emit_move(A1, ZERO, s);  // allocate nothing
    s << JAL << gc_collect_names[cgen_Memmgr] << endl;
    emit_addiu(SP, SP, 4, s);
    emit_load(ACC, 0, SP, s);
}

static void emit_gc_check(char* source, ostream& s)
{
    if (source != (char*)A1)
        emit_move(A1, source, s);
    s << JAL << "_gc_check" << endl;
}

///////////////////////////////////////////////////////////////////////////////
//
// coding strings, ints, and booleans
//
// Cool has three kinds of constants: strings, ints, and booleans.
// This section defines code generation for each type.
//
// All string constants are listed in the global "stringtable" and have
// type StringEntry.  StringEntry methods are defined both for String
// constant definitions and references.
//
// All integer constants are listed in the global "inttable" and have
// type IntEntry.  IntEntry methods are defined for Int
// constant definitions and references.
//
// Since there are only two Bool values, there is no need for a table.
// The two booleans are represented by instances of the class BoolConst,
// which defines the definition and reference methods for Bools.
//
///////////////////////////////////////////////////////////////////////////////

//
// Strings
//
void StringEntry::code_ref(ostream& s)
{
    s << STRCONST_PREFIX << index;
}

//
// Emit code for a constant String.
// You should fill in the code naming the dispatch table.
//

void StringEntry::code_def(ostream& s, int stringclasstag)
{
    IntEntryP lensym = inttable.add_int(len);

    // Add -1 eye catcher
    s << WORD << "-1" << endl;

    code_ref(s);
    s << LABEL                                                               // label
      << WORD << stringclasstag << endl                                      // tag
      << WORD << (DEFAULT_OBJFIELDS + STRING_SLOTS + (len + 4) / 4) << endl  // size
      << WORD;

    /***** Add dispatch information for class String ******/
    emit_disptable_ref(Str, s);
    s << endl;  // dispatch table
    s << WORD;
    lensym->code_ref(s);
    s << endl;                     // string length
    emit_string_constant(s, str);  // ascii string
    s << ALIGN;                    // align to word
}

//
// StrTable::code_string
// Generate a string object definition for every string constant in the
// stringtable.
//
void StrTable::code_string_table(ostream& s, int stringclasstag)
{
    for (List<StringEntry>* l = tbl; l; l = l->tl())
        l->hd()->code_def(s, stringclasstag);
}

//
// Ints
//
void IntEntry::code_ref(ostream& s)
{
    s << INTCONST_PREFIX << index;
}

//
// Emit code for a constant Integer.
// You should fill in the code naming the dispatch table.
//

void IntEntry::code_def(ostream& s, int intclasstag)
{
    // Add -1 eye catcher
    s << WORD << "-1" << endl;

    code_ref(s);
    s << LABEL                                            // label
      << WORD << intclasstag << endl                      // class tag
      << WORD << (DEFAULT_OBJFIELDS + INT_SLOTS) << endl  // object size
      << WORD;

    /***** Add dispatch information for class Int ******/
    emit_disptable_ref(Int, s);
    s << endl;                 // dispatch table
    s << WORD << str << endl;  // integer value
}

//
// IntTable::code_string_table
// Generate an Int object definition for every Int constant in the
// inttable.
//
void IntTable::code_string_table(ostream& s, int intclasstag)
{
    for (List<IntEntry>* l = tbl; l; l = l->tl())
        l->hd()->code_def(s, intclasstag);
}

//
// Bools
//
BoolConst::BoolConst(int i) : val(i)
{
    assert(i == 0 || i == 1);
}

void BoolConst::code_ref(ostream& s) const
{
    s << BOOLCONST_PREFIX << val;
}

std::string BoolConst::code_str() const
{
    return BOOLCONST_PREFIX + std::to_string(val);
}
//
// Emit code for a constant Bool.
// You should fill in the code naming the dispatch table.
//

void BoolConst::code_def(ostream& s, int boolclasstag)
{
    // Add -1 eye catcher
    s << WORD << "-1" << endl;

    code_ref(s);
    s << LABEL                                             // label
      << WORD << boolclasstag << endl                      // class tag
      << WORD << (DEFAULT_OBJFIELDS + BOOL_SLOTS) << endl  // object size
      << WORD;

    /***** Add dispatch information for class Bool ******/
    emit_disptable_ref(Bool, s);

    s << endl;                 // dispatch table
    s << WORD << val << endl;  // value (0 or 1)
}

//////////////////////////////////////////////////////////////////////////////
//
//  CgenClassTable methods
//
//////////////////////////////////////////////////////////////////////////////

//***************************************************
//
//  Emit code to start the .data segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_data()
{
    Symbol main = idtable.lookup_string(MAINNAME);
    Symbol string = idtable.lookup_string(STRINGNAME);
    Symbol integer = idtable.lookup_string(INTNAME);
    Symbol boolc = idtable.lookup_string(BOOLNAME);

    str << "\t.data\n" << ALIGN;
    //
    // The following global names must be defined first.
    //
    str << GLOBAL << CLASSNAMETAB << endl;
    str << GLOBAL;
    emit_protobj_ref(main, str);
    str << endl;
    str << GLOBAL;
    emit_protobj_ref(integer, str);
    str << endl;
    str << GLOBAL;
    emit_protobj_ref(string, str);
    str << endl;
    str << GLOBAL;
    falsebool.code_ref(str);
    str << endl;
    str << GLOBAL;
    truebool.code_ref(str);
    str << endl;
    str << GLOBAL << INTTAG << endl;
    str << GLOBAL << BOOLTAG << endl;
    str << GLOBAL << STRINGTAG << endl;

    //
    // We also need to know the tag of the Int, String, and Bool classes
    // during code generation.
    //
    str << INTTAG << LABEL << WORD << intclasstag << endl;
    str << BOOLTAG << LABEL << WORD << boolclasstag << endl;
    str << STRINGTAG << LABEL << WORD << stringclasstag << endl;
}

//***************************************************
//
//  Emit code to start the .text segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_text()
{
    str << GLOBAL << HEAP_START << endl << HEAP_START << LABEL << WORD << 0 << endl << "\t.text" << endl << GLOBAL;
    emit_init_ref(idtable.add_string("Main"), str);
    str << endl << GLOBAL;
    emit_init_ref(idtable.add_string("Int"), str);
    str << endl << GLOBAL;
    emit_init_ref(idtable.add_string("String"), str);
    str << endl << GLOBAL;
    emit_init_ref(idtable.add_string("Bool"), str);
    str << endl << GLOBAL;
    emit_method_ref(idtable.add_string("Main"), idtable.add_string("main"), str);
    str << endl;
}

void CgenClassTable::code_bools(int boolclasstag)
{
    falsebool.code_def(str, boolclasstag);
    truebool.code_def(str, boolclasstag);
}

void CgenClassTable::code_select_gc()
{
    //
    // Generate GC choice constants (pointers to GC functions)
    //
    str << GLOBAL << "_MemMgr_INITIALIZER" << endl;
    str << "_MemMgr_INITIALIZER:" << endl;
    str << WORD << gc_init_names[cgen_Memmgr] << endl;
    str << GLOBAL << "_MemMgr_COLLECTOR" << endl;
    str << "_MemMgr_COLLECTOR:" << endl;
    str << WORD << gc_collect_names[cgen_Memmgr] << endl;
    str << GLOBAL << "_MemMgr_TEST" << endl;
    str << "_MemMgr_TEST:" << endl;
    str << WORD << (cgen_Memmgr_Test == GC_TEST) << endl;
}

//********************************************************
//
// Emit code to reserve space for and initialize all of
// the constants.  Class names should have been added to
// the string table (in the supplied code, is is done
// during the construction of the inheritance graph), and
// code for emitting string constants as a side effect adds
// the string's length to the integer table.  The constants
// are emmitted by running through the stringtable and inttable
// and producing code for each entry.
//
//********************************************************

void CgenClassTable::code_constants()
{
    //
    // Add constants that are required by the code generator.
    //
    stringtable.add_string("");
    inttable.add_string("0");

    stringtable.code_string_table(str, stringclasstag);
    inttable.code_string_table(str, intclasstag);
    code_bools(boolclasstag);
}

CgenClassTable* CgenClassTable::singleton = nullptr;
CgenClassTable::CgenClassTable(Classes classes, ostream& s) : nds(NULL), str(s)
{
    singleton = this;

    enterscope();
    if (cgen_debug)
        cout << "Building CgenClassTable" << endl;
    install_basic_classes();
    install_classes(classes);
    build_inheritance_tree();

    stringclasstag = m_mapClassTag[std::string(Str->get_string())] /* Change to your String class tag here */;
    intclasstag = m_mapClassTag[std::string(Int->get_string())] /* Change to your Int class tag here */;
    boolclasstag = m_mapClassTag[std::string(Bool->get_string())] /* Change to your Bool class tag here */;

    code();
    exitscope();
}

void CgenClassTable::install_basic_classes()
{
    // The tree package uses these globals to annotate the classes built below.
    // curr_lineno  = 0;
    Symbol filename = stringtable.add_string("<basic class>");

    //
    // A few special class names are installed in the lookup table but not
    // the class list.  Thus, these classes exist, but are not part of the
    // inheritance hierarchy.
    // No_class serves as the parent of Object and the other special classes.
    // SELF_TYPE is the self class; it cannot be redefined or inherited.
    // prim_slot is a class known to the code generator.
    //
    addid(No_class, new CgenNode(class_(No_class, No_class, nil_Features(), filename), Basic, this));
    addid(SELF_TYPE, new CgenNode(class_(SELF_TYPE, No_class, nil_Features(), filename), Basic, this));
    addid(prim_slot, new CgenNode(class_(prim_slot, No_class, nil_Features(), filename), Basic, this));

    //
    // The Object class has no parent class. Its methods are
    //        cool_abort() : Object    aborts the program
    //        type_name() : Str        returns a string representation of class name
    //        copy() : SELF_TYPE       returns a copy of the object
    //
    // There is no need for method bodies in the basic classes---these
    // are already built in to the runtime system.
    //
    install_class(new CgenNode(
        class_(Object, No_class,
               append_Features(append_Features(single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
                                               single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
                               single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
               filename),
        Basic, this));

    //
    // The IO class inherits from Object. Its methods are
    //        out_string(Str) : SELF_TYPE          writes a string to the output
    //        out_int(Int) : SELF_TYPE               "    an int    "  "     "
    //        in_string() : Str                    reads a string from the input
    //        in_int() : Int                         "   an int     "  "     "
    //
    install_class(new CgenNode(
        class_(IO, Object,
               append_Features(
                   append_Features(
                       append_Features(
                           single_Features(method(out_string, single_Formals(formal(arg, Str)), SELF_TYPE, no_expr())),
                           single_Features(method(out_int, single_Formals(formal(arg, Int)), SELF_TYPE, no_expr()))),
                       single_Features(method(in_string, nil_Formals(), Str, no_expr()))),
                   single_Features(method(in_int, nil_Formals(), Int, no_expr()))),
               filename),
        Basic, this));

    //
    // The Int class has no methods and only a single attribute, the
    // "val" for the integer.
    //
    install_class(
        new CgenNode(class_(Int, Object, single_Features(attr(val, prim_slot, no_expr())), filename), Basic, this));

    //
    // Bool also has only the "val" slot.
    //
    install_class(
        new CgenNode(class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())), filename), Basic, this));

    //
    // The class Str has a number of slots and operations:
    //       val                                  ???
    //       str_field                            the string itself
    //       length() : Int                       length of the string
    //       concat(arg: Str) : Str               string concatenation
    //       substr(arg: Int, arg2: Int): Str     substring
    //
    install_class(new CgenNode(
        class_(
            Str, Object,
            append_Features(
                append_Features(append_Features(append_Features(single_Features(attr(val, Int, no_expr())),
                                                                single_Features(attr(str_field, prim_slot, no_expr()))),
                                                single_Features(method(length, nil_Formals(), Int, no_expr()))),
                                single_Features(method(concat, single_Formals(formal(arg, Str)), Str, no_expr()))),
                single_Features(
                    method(substr, append_Formals(single_Formals(formal(arg, Int)), single_Formals(formal(arg2, Int))),
                           Str, no_expr()))),
            filename),
        Basic, this));
}

// CgenClassTable::install_class
// CgenClassTable::install_classes
//
// install_classes enters a list of classes in the symbol table.
//
void CgenClassTable::install_class(CgenNodeP nd)
{
    Symbol name = nd->get_name();

    if (probe(name))
    {
        return;
    }

    // The class name is legal, so add it to the list of classes
    // and the symbol table.
    nds = new List<CgenNode>(nd, nds);
    addid(name, nd);
    m_mapClassTag[std::string(name->get_string())] = GetNextClassTag();
}

void CgenClassTable::install_classes(Classes cs)
{
    for (int i = cs->first(); cs->more(i); i = cs->next(i))
        install_class(new CgenNode(cs->nth(i), NotBasic, this));
}

//
// CgenClassTable::build_inheritance_tree
//
void CgenClassTable::build_inheritance_tree()
{
    for (List<CgenNode>* l = nds; l; l = l->tl())
        set_relations(l->hd());
}

//
// CgenClassTable::set_relations
//
// Takes a CgenNode and locates its, and its parent's, inheritance nodes
// via the class table.  Parent and child pointers are added as appropriate.
//
void CgenClassTable::set_relations(CgenNodeP nd)
{
    CgenNode* parent_node = probe(nd->get_parent());
    nd->set_parentnd(parent_node);
    parent_node->add_child(nd);
}

void CgenNode::add_child(CgenNodeP n)
{
    children = new List<CgenNode>(n, children);
}

void CgenNode::set_parentnd(CgenNodeP p)
{
    assert(parentnd == NULL);
    assert(p != NULL);
    parentnd = p;
}

void CgenClassTable::code()
{
    if (cgen_debug)
        cout << "coding global data" << endl;
    code_global_data();

    if (cgen_debug)
        cout << "choosing gc" << endl;
    code_select_gc();

    if (cgen_debug)
        cout << "coding constants" << endl;
    code_constants();

    //                 Add your code to emit
    //                   - prototype objects
    //                   - class_nameTab
    //                   - dispatch tables
    //

    // 构造所有类的模板 从Object一直递归下去
    code_prototype_object(root());

    // 构造classNameTable
    code_class_nameTab();

    // 构造dispatchTables
    code_dispatch_tables(root());

    if (cgen_debug)
        cout << "coding global text" << endl;
    code_global_text();

    //                 Add your code to emit
    //                   - object initializer
    //                   - the class methods
    //                   - etc...

    // 生成Object_Initializer
    code_object_initializer(root());

    // 生成各个方法
    code_class_methods(root());
}

CgenNodeP CgenClassTable::root()
{
    return probe(Object);
}

void CgenClassTable::code_prototype_object(CgenNodeP node)
{
    assert(node);

    // Add -1 eye catcher
    str << WORD << "-1" << endl;

    std::vector<attr_class*> vecAttr = get_all_attr(node);
    emit_protobj_ref(node->get_name(), str);
    str << LABEL;
    str << WORD << m_mapClassTag[std::string(node->get_name()->get_string())] << endl;  // class tag
    str << WORD << vecAttr.size() + 3 << endl;                                          // class size
    str << WORD;
    emit_disptable_ref(node->get_name(), str);  // dispatch pointer
    str << endl;
    for (size_t i = 0; i < vecAttr.size(); ++i)
    {
        if (vecAttr[i]->type_decl == Bool)
        {
            str << WORD << BoolConst(0).code_str() << endl;
        }
        else if (vecAttr[i]->type_decl == Int)
        {
            IntEntry* int0 = inttable.lookup_string("0");
            str << WORD;
            int0->code_ref(str);
            str << endl;
        }
        else if (vecAttr[i]->type_decl == Str)
        {
            StringEntry* strEmpty = stringtable.lookup_string("");
            str << WORD;
            strEmpty->code_ref(str);
            str << endl;
        }
        else
        {
            str << WORD << "0" << endl;
        }
    }

    List<CgenNode>* children = node->get_children();
    for (auto iter = children; iter; iter = iter->tl())
    {
        code_prototype_object(iter->hd());
    }
}

std::vector<attr_class*> CgenClassTable::get_all_attr(CgenNodeP node)
{
    std::vector<attr_class*> result;
    if (!node)
    {
        return result;
    }

    auto features = node->features;
    for (int i = features->first(); features->more(i); i = features->next(i))
    {
        auto pFeature = features->nth(i);

        attr_class* pAttr = dynamic_cast<attr_class*>(pFeature);
        if (pAttr)
        {
            result.push_back(pAttr);
        }
    }

    std::vector<attr_class*> parentAttr = get_all_attr(node->get_parentnd());
    result.insert(result.begin(), parentAttr.begin(), parentAttr.end());
    return result;
}

std::vector<attr_class*> CgenClassTable::get_self_attr(CgenNodeP node)
{
    std::vector<attr_class*> result;

    auto features = node->features;
    for (int i = features->first(); features->more(i); i = features->next(i))
    {
        auto pFeature = features->nth(i);

        attr_class* pAttr = dynamic_cast<attr_class*>(pFeature);
        if (pAttr)
        {
            result.push_back(pAttr);
        }
    }

    return result;
}

std::vector<std::pair<std::string, method_class*>> CgenClassTable::get_all_method(CgenNodeP node)
{
    std::vector<std::pair<std::string, method_class*>> result;
    if (!node)
    {
        return result;
    }

    auto features = node->features;
    for (int i = features->first(); features->more(i); i = features->next(i))
    {
        auto pFeature = features->nth(i);

        method_class* pMethod = dynamic_cast<method_class*>(pFeature);
        if (pMethod)
        {
            result.push_back(std::make_pair(std::string(node->get_name()->get_string()), pMethod));
        }
    }

    std::vector<std::pair<std::string, method_class*>> parentMethod = get_all_method(node->get_parentnd());

    std::set<std::string> setParentMethodName;
    std::set<std::string> setChildMethodName;

    for (const auto& parent : parentMethod)
    {
        std::string parentMethodName(parent.second->name->get_string());
        setParentMethodName.insert(parentMethodName);
    }

    for (const auto& child : result)
    {
        std::string childMethodName(child.second->name->get_string());
        setChildMethodName.insert(childMethodName);
    }

    std::vector<std::string> vecOverrideMethodName;
    for (const auto& parent : setParentMethodName)
    {
        for (const auto& child : setChildMethodName)
        {
            if (parent == child)
            {
                vecOverrideMethodName.push_back(parent);
            }
        }
    }

    for (const auto& overrideMethod : vecOverrideMethodName)
    {
        for (auto iter = result.begin(); iter != result.end();)
        {
            std::string sName(iter->second->name->get_string());
            if (sName == overrideMethod)
            {
                // 覆盖原先的函数
                for (size_t i = 0; i < parentMethod.size(); ++i)
                {
                    std::string sOldName(parentMethod[i].second->name->get_string());
                    if (sName == sOldName)
                    {
                        parentMethod[i] = *iter;
                        iter = result.erase(iter);
                        break;
                    }
                }
                continue;
            }
            iter++;
        }
    }

    // result 剩下的都是没有重载的
    parentMethod.insert(parentMethod.end(), result.begin(), result.end());
    return parentMethod;
}

std::vector<method_class*> CgenClassTable::get_self_method(CgenNodeP node)
{
    std::vector<method_class*> result;
    if (!node)
    {
        return result;
    }

    auto features = node->features;
    for (int i = features->first(); features->more(i); i = features->next(i))
    {
        auto pFeature = features->nth(i);

        method_class* pMethod = dynamic_cast<method_class*>(pFeature);
        if (pMethod)
        {
            result.push_back(pMethod);
        }
    }
    return result;
}

void CgenClassTable::code_class_nameTab()
{
    std::map<int, std::string> mapTag2ClassName;
    for (const auto& iter : m_mapClassTag)
    {
        mapTag2ClassName[iter.second] = iter.first;
    }

    str << CLASSNAMETAB << LABEL;

    for (const auto& iter : mapTag2ClassName)
    {
        std::string className = iter.second;
        StringEntry* entry = stringtable.lookup_string((char*)className.c_str());
        assert(entry);
        str << WORD;
        entry->code_ref(str);
        str << endl;
    }
}

void CgenClassTable::code_dispatch_tables(CgenNodeP node)
{
    emit_disptable_ref(node->get_name(), str);
    str << LABEL;
    // 各种函数
    std::vector<std::pair<std::string, method_class*>> methods = get_all_method(node);
    for (const auto& method : methods)
    {
        str << WORD;
        str << method.first << METHOD_SEP << method.second->name << endl;
    }

    List<CgenNode>* children = node->get_children();
    for (auto iter = children; iter; iter = iter->tl())
    {
        code_dispatch_tables(iter->hd());
    }
}

void CgenClassTable::code_object_initializer(CgenNodeP node)
{
    emit_init_ref(node->name, str);
    str << LABEL;

    emit_callee_begin();

    // 自身逻辑

    // 保存self
    emit_move(SELF, ACC, str);

    // 1. 调用父类构造函数
    if (node->name != Object)
    {
        str << JAL;
        emit_init_ref(node->parent, str);
        str << endl;
    }

    // 初始化各个字段
    std::vector<attr_class*> vecSelfAttr = get_self_attr(node);
    std::vector<attr_class*> vecAllAttr = get_all_attr(node);
    size_t uFirstAttrIndex = vecAllAttr.size() - vecSelfAttr.size();
    for (size_t i = 0; i < vecSelfAttr.size(); ++i)
    {
        assert(vecSelfAttr[i]->init);
        if (!dynamic_cast<no_expr_class*>(vecSelfAttr[i]->init))
        {
            vecSelfAttr[i]->init->code(str);
            emit_store(ACC, DEFAULT_OBJFIELDS + uFirstAttrIndex + i, SELF, str);
        }
    }

    // 恢复self
    emit_move(ACC, SELF, str);

    emit_callee_end(0);

    List<CgenNode>* children = node->get_children();
    for (auto iter = children; iter; iter = iter->tl())
    {
        code_object_initializer(iter->hd());
    }
}

void CgenClassTable::emit_callee_begin()
{
    // 保存旧FP
    emit_push(FP, str);

    // 设置新$fp
    emit_move(FP, SP, str);

    // 保存self
    emit_push(SELF, str);

    // push $ra
    emit_push(RA, str);
}

void CgenClassTable::emit_callee_end(int iNum)
{
    // pop $ra
    emit_pop(RA, str);

    // 恢复self
    emit_pop(SELF, str);

    // 恢复$fp
    emit_pop(FP, str);

    // pop 所有栈变量
    if (iNum != 0)
    {
        emit_addiu(SP, SP, iNum * 4, str);
    }

    // RET
    emit_return(str);
}

void CgenClassTable::code_class_methods(CgenNodeP node)
{
    // 只生成非basic类的方法
    if (!node->basic())
    {
        m_currentClass = node;
        std::vector<method_class*> vecMethod = get_self_method(node);
        for (const auto& iter : vecMethod)
        {
            // 记录当前处理函数
            m_currentMethod = iter;
            emit_method_ref(node->name, iter->name, str);
            str << LABEL;
            emit_callee_begin();
            iter->expr->code(str);
            emit_callee_end(iter->formals->len());
        }
    }

    List<CgenNode>* children = node->get_children();
    for (auto iter = children; iter; iter = iter->tl())
    {
        code_class_methods(iter->hd());
    }
}

int CgenClassTable::GetDispatchOffset(Symbol Class, Symbol function)
{
    if (std::string(Class->get_string()) == std::string("SELF_TYPE"))
    {
        Class = m_currentClass->name;
    }

    CgenNodeP node = lookup(Class);
    if (!node)
    {
        str << "#not find " << Class->get_string() << endl;
        return 0;
    }
    std::vector<std::pair<std::string, method_class*>> vecMethod = get_all_method(node);
    int iOffset = 0;
    for (const auto& method : vecMethod)
    {
        if (method.second->name == function)
        {
            return iOffset;
        }
        else
        {
            iOffset++;
        }
    }
    str << "# not find " << function->get_string() << ", Class: " << Class->get_string() << endl;
    return 0;
}

bool CgenClassTable::emit_Left_value_Addr(Symbol ValueName)
{
    // 在成员变量中查找
    int iAttrOffset = GetAttrOffset(ValueName);
    if (iAttrOffset > 0)
    {
        emit_addiu(T1, SELF, (iAttrOffset + DEFAULT_OBJFIELDS - 1) * WORD_SIZE, str);
        return true;
    }

    // 在函数参数中查找
    int iParamOffset = GetParamOffset(ValueName);
    if (iParamOffset > 0)
    {
        emit_addiu(T1, FP, (iParamOffset + 1) * WORD_SIZE, str);
        return true;
    }

    // 在栈变量中查找
    int iVarOffset = GetVarOffset(ValueName);
    if (iVarOffset > 0)
    {
        emit_addiu(T1, SP, iVarOffset * WORD_SIZE, str);
        return true;
    }

    return false;
}

int CgenClassTable::GetAttrOffset(Symbol ObjName)
{
    std::vector<attr_class*> vecAttr = get_all_attr(m_currentClass);
    int iOffset = 1;
    for (const auto& pAttr : vecAttr)
    {
        if (pAttr->name == ObjName)
        {
            return iOffset;
        }
        else
        {
            ++iOffset;
        }
    }
    return 0;
}

int CgenClassTable::GetParamOffset(Symbol ParamName)
{
    std::vector<formal_class*> vecParam;
    Formals formals = m_currentMethod->formals;
    for (int i = formals->first(); formals->more(i); i = formals->next(i))
    {
        vecParam.push_back(dynamic_cast<formal_class*>(formals->nth(i)));
    }

    // 翻转
    std::reverse(vecParam.begin(), vecParam.end());

    int iOffset = 1;
    for (const auto& param : vecParam)
    {
        if (param->name == ParamName)
        {
            return iOffset;
        }
        else
        {
            ++iOffset;
        }
    }

    return 0;
}
void CgenClassTable::AddStackVar(Symbol VarName)
{
    m_StackVar.push_back(VarName);
}

void CgenClassTable::DelStackVar()
{
    assert(!m_StackVar.empty());
    m_StackVar.pop_back();
}
int CgenClassTable::GetVarOffset(Symbol VarName)
{
    auto tmp = m_StackVar;
    std::reverse(tmp.begin(), tmp.end());
    int iOffset = 1;
    for (const auto& var : tmp)
    {
        if (var == VarName)
        {
            return iOffset;
        }
        else
        {
            ++iOffset;
        }
    }
    return 0;
}

///////////////////////////////////////////////////////////////////////
//
// CgenNode methods
//
///////////////////////////////////////////////////////////////////////

CgenNode::CgenNode(Class_ nd, Basicness bstatus, CgenClassTableP ct)
    : class__class((const class__class&)*nd), parentnd(NULL), children(NULL), basic_status(bstatus)
{
    stringtable.add_string(name->get_string());  // Add class name to string table
}

//******************************************************************
//
//   Fill in the following methods to produce code for the
//   appropriate expression.  You may add or remove parameters
//   as you wish, but if you do, remember to change the parameters
//   of the declarations in `cool-tree.h'  Sample code for
//   constant integers, strings, and booleans are provided.
//
//*****************************************************************

void assign_class::code(ostream& s)
{
    s << "\t\t\t# assign_class::code" << endl;

    bool ok = CgenClassTable::GetInstance()->emit_Left_value_Addr(name);
    assert(ok);
    emit_push(T1, s);
    expr->code(s);
    emit_pop(T1, s);
    emit_store(ACC, 0, T1, s);
}

void static_dispatch_class::code(ostream& s)
{
    s << "\t\t\t# static_dispatch_class::code" << endl;
    for (int i = actual->first(); actual->more(i); i = actual->next(i))
    {
        actual->nth(i)->code(s);
        emit_push(ACC, s);
    }
    expr->code(s);

    s << JAL;
    emit_method_ref(type_name, name, s);
    s << endl;
}

void dispatch_class::code(ostream& s)
{
    s << "\t\t\t# dispatch_class::code" << endl;

    for (int i = actual->first(); actual->more(i); i = actual->next(i))
    {
        actual->nth(i)->code(s);
        emit_push(ACC, s);
        CgenClassTable::GetInstance()->AddStackVar(nullptr);
    }
    for (int i = actual->first(); actual->more(i); i = actual->next(i))
    {
        CgenClassTable::GetInstance()->DelStackVar();
    }

    // 通过虚表跳转
    // 1. 拿到函数偏移
    int iOffset = CgenClassTable::GetInstance()->GetDispatchOffset(expr->get_type(), name);
    s << "# offset = " << iOffset << endl;

    expr->code(s);
    emit_push(ACC, s);
    // 2. 拿到具体函数指针(T1)
    emit_load(ACC, 2, ACC, s);
    emit_load(T1, iOffset, ACC, s);

    emit_pop(ACC, s);
    emit_jal(T1, s);
}

void cond_class::code(ostream& s)
{
    s << "\t\t\t# cond_class::code" << endl;
    pred->code(s);
    int iTrueLable = CgenClassTable::GetInstance()->GetNextLable();
    int iFalseLable = CgenClassTable::GetInstance()->GetNextLable();
    int iEndLable = CgenClassTable::GetInstance()->GetNextLable();
    emit_load_address(T1, (char*)BoolConst(0).code_str().c_str(), s);
    emit_bne(ACC, T1, iTrueLable, s);
    emit_label_def(iFalseLable, s);
    else_exp->code(s);
    emit_branch(iEndLable, s);
    emit_label_def(iTrueLable, s);
    then_exp->code(s);
    emit_label_def(iEndLable, s);
}

void loop_class::code(ostream& s)
{
    s << "\t\t\t# loop_class::code" << endl;
    int iBeginLabel = CgenClassTable::GetInstance()->GetNextLable();
    int iEndLabel = CgenClassTable::GetInstance()->GetNextLable();
    emit_label_def(iBeginLabel, s);
    pred->code(s);
    emit_load_address(T1, (char*)BoolConst(0).code_str().c_str(), s);
    emit_beq(ACC, T1, iEndLabel, s);
    body->code(s);
    emit_branch(iBeginLabel, s);
    emit_label_def(iEndLabel, s);
}

void typcase_class::code(ostream& s)
{
    s << "\t\t\t# typcase_class::code" << endl;
}

void block_class::code(ostream& s)
{
    s << "\t\t\t# block_class::code" << endl;
    for (int i = body->first(); body->more(i); i = body->next(i))
        body->nth(i)->code(s);
}

void let_class::code(ostream& s)
{
    s << "\t\t\t# let_class::code" << endl;

    if (dynamic_cast<no_expr_class*>(init))
    {
        // 没有初始化栈变量
        if (type_decl == Str)
        {
            emit_load_string(ACC, stringtable.lookup_string(""), s);
        }
        else if (type_decl == Int)
        {
            emit_load_int(ACC, inttable.lookup_string("0"), s);
        }
        else if (type_decl == Bool)
        {
            emit_load_bool(ACC, BoolConst(0), s);
        }
    }
    else
    {
        init->code(s);
    }

    emit_push(ACC, s);
    CgenClassTable::GetInstance()->AddStackVar(identifier);

    body->code(s);

    emit_pop(ACC, s);
    CgenClassTable::GetInstance()->DelStackVar();
}

void plus_class::code(ostream& s)
{
    s << "\t\t\t# plus_class::code" << endl;
    // 新建一个右值
    emit_load_int(ACC, inttable.lookup_string("0"), s);
    emit_jal("Object.copy", s);
    emit_push(ACC, s);

    CgenClassTable::GetInstance()->AddStackVar(nullptr);

    e1->code(s);
    emit_load(T1, 3, ACC, s);
    emit_push(T1, s);

    e2->code(s);
    emit_load(T2, 3, ACC, s);
    emit_pop(T1, s);
    emit_add(T1, T1, T2, s);  // result value

    // 获取临时右值
    emit_pop(ACC, s);
    CgenClassTable::GetInstance()->DelStackVar();
    emit_store(T1, 3, ACC, s);
}

void sub_class::code(ostream& s)
{
    s << "\t\t\t# sub_class::code" << endl;

    // 新建一个右值
    emit_load_int(ACC, inttable.lookup_string("0"), s);
    emit_jal("Object.copy", s);
    emit_push(ACC, s);

    CgenClassTable::GetInstance()->AddStackVar(nullptr);

    e1->code(s);
    emit_load(T1, 3, ACC, s);
    emit_push(T1, s);

    e2->code(s);
    emit_load(T2, 3, ACC, s);
    emit_pop(T1, s);
    emit_sub(T1, T1, T2, s);  // result value

    // 获取临时右值
    emit_pop(ACC, s);
    CgenClassTable::GetInstance()->DelStackVar();
    emit_store(T1, 3, ACC, s);
}

void mul_class::code(ostream& s)
{
    s << "\t\t\t# mul_class::code" << endl;

    // 新建一个右值
    emit_load_int(ACC, inttable.lookup_string("0"), s);
    emit_jal("Object.copy", s);
    emit_push(ACC, s);

    CgenClassTable::GetInstance()->AddStackVar(nullptr);

    e1->code(s);
    emit_load(T1, 3, ACC, s);
    emit_push(T1, s);

    e2->code(s);
    emit_load(T2, 3, ACC, s);
    emit_pop(T1, s);
    emit_mul(T1, T1, T2, s);  // result value

    // 获取临时右值
    emit_pop(ACC, s);
    CgenClassTable::GetInstance()->DelStackVar();
    emit_store(T1, 3, ACC, s);
}

void divide_class::code(ostream& s)
{
    s << "\t\t\t# divide_class::code" << endl;

    // 新建一个右值
    emit_load_int(ACC, inttable.lookup_string("0"), s);
    emit_jal("Object.copy", s);
    emit_push(ACC, s);

    CgenClassTable::GetInstance()->AddStackVar(nullptr);

    e1->code(s);
    emit_load(T1, 3, ACC, s);
    emit_push(T1, s);

    e2->code(s);
    emit_load(T2, 3, ACC, s);
    emit_pop(T1, s);
    emit_div(T1, T1, T2, s);  // result value

    // 获取临时右值
    emit_pop(ACC, s);
    CgenClassTable::GetInstance()->DelStackVar();
    emit_store(T1, 3, ACC, s);
}

void neg_class::code(ostream& s)
{
    s << "\t\t\t# neg_class::code" << endl;
    e1->code(s);
    emit_neg(ACC, ACC, s);
}

void lt_class::code(ostream& s)
{
    s << "\t\t\t# lt_class::code" << endl;
    e1->code(s);
    emit_push(ACC, s);
    e2->code(s);
    emit_pop(T1, s);
    int iTrueLable = CgenClassTable::GetInstance()->GetNextLable();
    int iFalseLable = CgenClassTable::GetInstance()->GetNextLable();
    int iEndLable = CgenClassTable::GetInstance()->GetNextLable();
    emit_blt(T1, ACC, iTrueLable, s);
    emit_label_def(iFalseLable, s);
    emit_load_bool(ACC, BoolConst(0), s);  // false: ACC = 0
    emit_branch(iEndLable, s);
    emit_label_def(iTrueLable, s);
    emit_load_bool(ACC, BoolConst(1), s);  // true: ACC = 1
    emit_label_def(iEndLable, s);
}

void eq_class::code(ostream& s)
{
    s << "\t\t\t# eq_class::code" << endl;

    e1->code(s);
    emit_push(ACC, s);
    e2->code(s);
    emit_move(T2, ACC, s);
    emit_pop(T1, s);
    emit_load_bool(ACC, BoolConst(1), s);
    emit_load_bool(A1, BoolConst(0), s);
    emit_jal("equality_test", s);
    // 如果相等,A0 = true, 否则A0 = false

    int iTrueLable = CgenClassTable::GetInstance()->GetNextLable();
    int iFalseLable = CgenClassTable::GetInstance()->GetNextLable();
    int iEndLable = CgenClassTable::GetInstance()->GetNextLable();
    emit_load_bool(T1, BoolConst(1), s);
    emit_beq(T1, ACC, iTrueLable, s);
    emit_label_def(iFalseLable, s);
    emit_load_bool(ACC, BoolConst(0), s);  // false: ACC = 0
    emit_branch(iEndLable, s);
    emit_label_def(iTrueLable, s);
    emit_load_bool(ACC, BoolConst(1), s);  // true: ACC = 1
    emit_label_def(iEndLable, s);
}

void leq_class::code(ostream& s)
{
    s << "\t\t\t# leq_class::code" << endl;

    e1->code(s);
    emit_push(ACC, s);
    e2->code(s);
    emit_pop(T1, s);
    int iTrueLable = CgenClassTable::GetInstance()->GetNextLable();
    int iFalseLable = CgenClassTable::GetInstance()->GetNextLable();
    int iEndLable = CgenClassTable::GetInstance()->GetNextLable();
    emit_bleq(T1, ACC, iTrueLable, s);
    emit_label_def(iFalseLable, s);
    emit_load_bool(ACC, BoolConst(0), s);  // false: ACC = 0
    emit_branch(iEndLable, s);
    emit_label_def(iTrueLable, s);
    emit_load_bool(ACC, BoolConst(1), s);  // true: ACC = 1
    emit_label_def(iEndLable, s);
}

void comp_class::code(ostream& s)
{
    s << "\t\t\t# comp_class::code" << endl;

    e1->code(s);
    int iTrueLable = CgenClassTable::GetInstance()->GetNextLable();
    int iFalseLable = CgenClassTable::GetInstance()->GetNextLable();
    int iEndLable = CgenClassTable::GetInstance()->GetNextLable();

    emit_beqz(ACC, iTrueLable, s);

    emit_label_def(iFalseLable, s);
    emit_load_bool(ACC, BoolConst(0), s);  // false: ACC = 0
    emit_branch(iEndLable, s);
    emit_label_def(iTrueLable, s);
    emit_load_bool(ACC, BoolConst(1), s);  // true: ACC = 1
    emit_label_def(iEndLable, s);
}

void int_const_class::code(ostream& s)
{
    s << "\t\t\t# int_const_class::code" << endl;
    //
    // Need to be sure we have an IntEntry *, not an arbitrary Symbol
    //
    emit_load_int(ACC, inttable.lookup_string(token->get_string()), s);
}

void string_const_class::code(ostream& s)
{
    s << "\t\t\t# string_const_class::code" << endl;
    emit_load_string(ACC, stringtable.lookup_string(token->get_string()), s);
}

void bool_const_class::code(ostream& s)
{
    s << "\t\t\t# bool_const_class::code" << endl;
    emit_load_bool(ACC, BoolConst(val), s);
}

void new__class::code(ostream& s)
{
    s << "\t\t\t# new__class::code" << endl;
    // 不用保存FP,因为被调方没有恢复fp的操作
    std::string sAddr = std::string(type_name->get_string()) + std::string(PROTOBJ_SUFFIX);
    emit_load_address(ACC, (char*)sAddr.c_str(), s);
    // 调用Object.copy后再调用对用的init方法
    emit_jal("Object.copy", s);
    emit_push(ACC, s);
    sAddr = std::string(type_name->get_string()) + std::string(CLASSINIT_SUFFIX);
    emit_jal((char*)sAddr.c_str(), s);
    emit_pop(ACC, s);
}

void isvoid_class::code(ostream& s)
{
    s << "\t\t\t# isvoid_class::code" << endl;
}

void no_expr_class::code(ostream& s)
{
    s << "\t\t\t# no_expr_class::code" << endl;
}

void object_class::code(ostream& s)
{
    s << "\t\t\t# object_class::code" << endl;

    bool ok = CgenClassTable::GetInstance()->emit_Left_value_Addr(name);
    if (!ok)
    {
        // self
        if (std::string(name->get_string()) == std::string("self"))
        {
            emit_move(ACC, SELF, s);
        }
        else
        {
            assert(false);
            s << "# unknow obj" << endl;
        }
    }
    else
    {
        emit_load(ACC, 0, T1, s);
    }
}
