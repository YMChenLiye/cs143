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

    Symbol GetLeastCommonAncestor(Symbol leftType, Symbol rightType);

    std::map<std::string, std::map<std::string, method_class*>> GetAllMethodTable();

    void PrintInherList();

    // 是否是子类
    bool IsSubType(Symbol childType, Symbol parentType, TypeCheckEnvironment& env);

    class__class* GetClassByName(const std::string& sClassName);

    std::vector<std::string> GetInheritList(class__class* selfClass);

private:
    void CheckOverrideMethod(const std::map<std::string, std::map<std::string, method_class*>> result);

    bool CompareMethodParameter(const method_class* leftMethod, const method_class* rightMethod);

    void CheckMethodReturnTypeAndParameterType(
        const std::map<std::string, std::map<std::string, method_class*>> result);

private:
    int semant_errors;
    Classes install_basic_classes();
    ostream& error_stream;

    std::map<std::string, class__class*> m_ClassMap;
};

#endif
