#ifndef SEMANT_H_
#define SEMANT_H_

#include <assert.h>
#include <iostream>
#include <map>
#include <string>
#include <vector>
#include "cool-tree.h"
#include "stringtab.h"
#include "symtab.h"
#include "list.h"

#define TRUE 1
#define FALSE 0

class ClassTable;
typedef ClassTable* ClassTableP;

//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
static Symbol arg, arg2, Bool, concat, cool_abort, copy, Int, in_int, in_string, IO, length, Main, main_meth, No_class,
    No_type, Object, out_int, out_string, prim_slot, self, SELF_TYPE, Str, str_field, substr, type_name, val;

// This is a structure that may be used to contain the semantic
// information such as the inheritance graph.  You may use it or not as
// you like: it is only here to provide a container for the supplied
// methods.

class ClassTable
{
public:
    ClassTable(Classes);
    int errors() { return semant_errors; }
    ostream& semant_error();
    ostream& semant_error(Class_ c);
    ostream& semant_error(Symbol filename, tree_node* t);

    class__class* GetLeastCommonAncestor(class__class* leftClass, class__class* rightClass);

    std::map<std::string, std::map<std::string, method_class*>> GetAllMethodTable();

    void PrintInherList();

private:
    std::vector<std::string> GetInheritList(class__class* selfClass);

private:
    int semant_errors;
    Classes install_basic_classes();
    ostream& error_stream;

    std::map<std::string, class__class*> m_ClassMap;
};

#endif
