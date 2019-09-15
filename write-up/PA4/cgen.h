#include <assert.h>
#include <stdio.h>
#include <map>
#include <string>
#include <vector>
#include "cool-tree.h"
#include "emit.h"
#include "symtab.h"

enum Basicness
{
    Basic,
    NotBasic
};
#define TRUE 1
#define FALSE 0

class CgenClassTable;
typedef CgenClassTable* CgenClassTableP;

class CgenNode;
typedef CgenNode* CgenNodeP;

class CgenClassTable : public SymbolTable<Symbol, CgenNode>
{
public:
    static CgenClassTable* singleton;
    static CgenClassTable* GetInstance() { return singleton; }
    int GetNextLable() { return ++m_iLable; }
    int GetDispatchOffset(Symbol Class, Symbol function);
    CgenNodeP m_currentClass = nullptr;
    method_class* m_currentMethod = nullptr;

    // 获取左值地址
    bool emit_Left_value_Addr(Symbol ValueName);

    void AddStackVar(Symbol VarName);
    void DelStackVar();

    int GetClassTag(Symbol type_decl);

private:
    List<CgenNode>* nds;
    ostream& str;
    int stringclasstag;
    int intclasstag;
    int boolclasstag;

    int m_iClassTag = 0;
    int m_iLable = 0;
    int GetNextClassTag() { return m_iClassTag++; }
    std::map<std::string, int> m_mapClassTag;

    std::vector<Symbol> m_StackVar;

    // The following methods emit code for
    // constants and global declarations.

    void code_global_data();
    void code_global_text();
    void code_bools(int);
    void code_select_gc();
    void code_constants();

    // 生成所有类的模板对象
    void code_prototype_object(CgenNodeP node);

    // 生成class_nameTab
    void code_class_nameTab();
    // 生成class_protTab
    void code_class_objTab();

    // 构造dispatchTables
    void code_dispatch_tables(CgenNodeP node);

    // 生成构造函数
    void code_object_initializer(CgenNodeP node);

    // 生成各种方法
    void code_class_methods(CgenNodeP node);

    // The following creates an inheritance graph from
    // a list of classes.  The graph is implemented as
    // a tree of `CgenNode', and class names are placed
    // in the base class symbol table.

    void install_basic_classes();
    void install_class(CgenNodeP nd);
    void install_classes(Classes cs);
    void build_inheritance_tree();
    void set_relations(CgenNodeP nd);

public:
    CgenClassTable(Classes, ostream& str);
    void code();
    CgenNodeP root();

private:
    std::vector<attr_class*> get_all_attr(CgenNodeP node);
    std::vector<attr_class*> get_self_attr(CgenNodeP node);
    std::vector<std::pair<std::string, method_class*>> get_all_method(CgenNodeP node);
    std::vector<method_class*> get_self_method(CgenNodeP node);

    void emit_callee_begin();
    void emit_callee_end(int iNum);

    int GetAttrOffset(Symbol ObjName);
    int GetParamOffset(Symbol ParamName);
    int GetVarOffset(Symbol VarName);
};

class CgenNode : public class__class
{
private:
    CgenNodeP parentnd;        // Parent of class
    List<CgenNode>* children;  // Children of class
    Basicness basic_status;    // `Basic' if class is basic
                               // `NotBasic' otherwise

public:
    CgenNode(Class_ c, Basicness bstatus, CgenClassTableP class_table);

    void add_child(CgenNodeP child);
    List<CgenNode>* get_children() { return children; }
    void set_parentnd(CgenNodeP p);
    CgenNodeP get_parentnd() { return parentnd; }
    int basic() { return (basic_status == Basic); }
};

class BoolConst
{
private:
    int val;

public:
    BoolConst(int);
    void code_def(ostream&, int boolclasstag);
    void code_ref(ostream&) const;
    std::string code_str() const;
};
