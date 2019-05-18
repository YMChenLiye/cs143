
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include "semant.h"
#include "utilities.h"
#include <algorithm>

extern int semant_debug;
extern char* curr_filename;

using std::vector;
using std::string;

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

//
// Initializing the predefined symbols.
//
static void initialize_constants(void)
{
    // cout << "initialize_constants" << endl;

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
    //   _no_class is a symbol that can't be the name of any user-defined class.
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

ClassTable::ClassTable(Classes classes) : semant_errors(0), error_stream(cerr)
{
    /* Fill this in */
    Classes basicClasses = install_basic_classes();
    classes = append_Classes(basicClasses, classes);

    for (int i = classes->first(); classes->more(i); i = classes->next(i))
    {
        class__class* Class = dynamic_cast<class__class*>(classes->nth(i));
        assert(Class);
        std::string sClassName = Class->GetClassName();
        if (m_ClassMap.find(sClassName) != m_ClassMap.end())
        {
            // 已经存在
            semant_error(Class);
            cerr << "Exsit Same Class Name :" << sClassName << endl;
        }
        else
        {
            m_ClassMap[sClassName] = Class;
            // cout << "Add Class: " << sClassName << endl;
        }

        // 不允许继承 Int Bool String
        if (string(Class->parent->get_string()) == "Int")
        {
            semant_error(Class);
            cerr << "Class: " << sClassName << " cannot inherit class Int." << endl;
        }
        else if (string(Class->parent->get_string()) == "Bool")
        {
            semant_error(Class);
            cerr << "Class: " << sClassName << " cannot inherit class Bool." << endl;
        }
        else if (string(Class->parent->get_string()) == "String")
        {
            semant_error(Class);
            cerr << "Class: " << sClassName << " cannot inherit class String." << endl;
        }
    }

    // 是否定义Main函数
    if (m_ClassMap.find("Main") == m_ClassMap.end())
    {
        semant_error();
        cerr << "Class Main is not defined." << endl;
    }
}

std::vector<std::string> ClassTable::GetInheritList(class__class* selfClass)
{
    std::vector<std::string> vecResult;
    class__class* currentClass = selfClass;
    while (currentClass)
    {
        vecResult.push_back(currentClass->GetClassName());
        const std::string sParentClassName = currentClass->GetParentClassName();
        if (m_ClassMap.find(sParentClassName) != m_ClassMap.end())
        {
            currentClass = m_ClassMap[sParentClassName];
        }
        else
        {
            if (sParentClassName != std::string(No_class->get_string()))
            {
                semant_error(currentClass);
                cerr << "Cant't find Class :" << sParentClassName << endl;
            }
            currentClass = nullptr;
        }
    }
    return vecResult;
}

Classes ClassTable::install_basic_classes()
{
    // The tree package uses these globals to annotate the classes built below.
    // curr_lineno  = 0;
    Symbol filename = stringtable.add_string("<basic class>");

    // The following demonstrates how to create dummy parse trees to
    // refer to basic Cool classes.  There's no need for method
    // bodies -- these are already built into the runtime system.

    // IMPORTANT: The results of the following expressions are
    // stored in local variables.  You will want to do something
    // with those variables at the end of this method to make this
    // code meaningful.

    //
    // The Object class has no parent class. Its methods are
    //        abort() : Object    aborts the program
    //        type_name() : Str   returns a string representation of class name
    //        copy() : SELF_TYPE  returns a copy of the object
    //
    // There is no need for method bodies in the basic classes---these
    // are already built in to the runtime system.

    Class_ Object_class =
        class_(Object, No_class,
               append_Features(append_Features(single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
                                               single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
                               single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
               filename);

    //
    // The IO class inherits from Object. Its methods are
    //        out_string(Str) : SELF_TYPE       writes a string to the output
    //        out_int(Int) : SELF_TYPE            "    an int    "  "     "
    //        in_string() : Str                 reads a string from the input
    //        in_int() : Int                      "   an int     "  "     "
    //
    Class_ IO_class =
        class_(IO, Object,
               append_Features(
                   append_Features(
                       append_Features(
                           single_Features(method(out_string, single_Formals(formal(arg, Str)), SELF_TYPE, no_expr())),
                           single_Features(method(out_int, single_Formals(formal(arg, Int)), SELF_TYPE, no_expr()))),
                       single_Features(method(in_string, nil_Formals(), Str, no_expr()))),
                   single_Features(method(in_int, nil_Formals(), Int, no_expr()))),
               filename);

    //
    // The Int class has no methods and only a single attribute, the
    // "val" for the integer.
    //
    Class_ Int_class = class_(Int, Object, single_Features(attr(val, prim_slot, no_expr())), filename);

    //
    // Bool also has only the "val" slot.
    //
    Class_ Bool_class = class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())), filename);

    //
    // The class Str has a number of slots and operations:
    //       val                                  the length of the string
    //       str_field                            the string itself
    //       length() : Int                       returns length of the string
    //       concat(arg: Str) : Str               performs string concatenation
    //       substr(arg: Int, arg2: Int): Str     substring selection
    //
    Class_ Str_class = class_(
        Str, Object,
        append_Features(
            append_Features(append_Features(append_Features(single_Features(attr(val, Int, no_expr())),
                                                            single_Features(attr(str_field, prim_slot, no_expr()))),
                                            single_Features(method(length, nil_Formals(), Int, no_expr()))),
                            single_Features(method(concat, single_Formals(formal(arg, Str)), Str, no_expr()))),
            single_Features(method(substr,
                                   append_Formals(single_Formals(formal(arg, Int)), single_Formals(formal(arg2, Int))),
                                   Str, no_expr()))),
        filename);

    Classes basicClasses = single_Classes(Object_class);
    basicClasses = append_Classes(basicClasses, single_Classes(IO_class));
    basicClasses = append_Classes(basicClasses, single_Classes(Int_class));
    basicClasses = append_Classes(basicClasses, single_Classes(Bool_class));
    basicClasses = append_Classes(basicClasses, single_Classes(Str_class));
    return basicClasses;
}

////////////////////////////////////////////////////////////////////
//
// semant_error is an overloaded function for reporting errors
// during semantic analysis.  There are three versions:
//
//    ostream& ClassTable::semant_error()
//
//    ostream& ClassTable::semant_error(Class_ c)
//       print line number and filename for `c'
//
//    ostream& ClassTable::semant_error(Symbol filename, tree_node *t)
//       print a line number and filename
//
///////////////////////////////////////////////////////////////////

ostream& ClassTable::semant_error(Class_ c)
{
    return semant_error(c->get_filename(), c);
}

ostream& ClassTable::semant_error(Symbol filename, tree_node* t)
{
    error_stream << filename << ":" << t->get_line_number() << ": ";
    return semant_error();
}

ostream& ClassTable::semant_error()
{
    semant_errors++;
    return error_stream;
}

class__class* ClassTable::GetLeastCommonAncestor(class__class* leftClass, class__class* rightClass)
{
    const std::vector<std::string> vecLeftInheritList = GetInheritList(leftClass);
    const std::vector<std::string> vecRightInheritList = GetInheritList(rightClass);

    const auto& vecShort =
        vecLeftInheritList.size() <= vecRightInheritList.size() ? vecLeftInheritList : vecRightInheritList;

    const auto& vecLong =
        vecLeftInheritList.size() <= vecRightInheritList.size() ? vecRightInheritList : vecLeftInheritList;

    std::string sAncestor;
    assert(!vecShort.empty());

    for (size_t i = 0; i < vecShort.size(); ++i)
    {
        const std::string& sShort = vecShort[vecShort.size() - 1 - i];
        const std::string& sLong = vecLong[vecLong.size() - 1 - i];
        if (sShort == sLong)
        {
            sAncestor = sShort;
        }
        else
        {
            break;
        }
    }

    if (m_ClassMap.find(sAncestor) != m_ClassMap.end())
    {
        return m_ClassMap[sAncestor];
    }

    assert(false);

    return nullptr;
}

Symbol ClassTable::GetLeastCommonAncestor(Symbol leftType, Symbol rightType)
{
    auto iterLeft = m_ClassMap.find(leftType->get_string());
    auto iterRight = m_ClassMap.find(rightType->get_string());
    if (iterLeft != m_ClassMap.end() && iterRight != m_ClassMap.end())
    {
        class__class* pClass = GetLeastCommonAncestor(iterLeft->second, iterRight->second);
        if (pClass)
        {
            return pClass->name;
        }
    }
    return nullptr;
}

std::map<std::string, std::map<std::string, method_class*>> ClassTable::GetAllMethodTable()
{
    std::map<std::string, std::map<std::string, method_class*>> result;
    for (const auto& classs : m_ClassMap)
    {
        std::map<std::string, method_class*> mapMethod;
        const std::string sClassName = classs.first;
        class__class* pClass = classs.second;
        std::vector<method_class*> vecMethod = pClass->GetAllMethod();
        for (const auto& pMethod : vecMethod)
        {
            const std::string sMethodName = pMethod->GetMethodName();
            if (mapMethod.find(sMethodName) != mapMethod.end())
            {
                semant_error(pClass);
                cerr << "Same Method: " << sMethodName << endl;
            }
            else
            {
                // cout << "\tAdd Method: " << sMethodName << endl;
                mapMethod[sMethodName] = pMethod;
            }
        }
        // cout << "Add Class: " << sClassName << "'s Method Done. size = " << mapMethod.size() << endl;

        result[sClassName] = mapMethod;
    }

    return result;
}

void ClassTable::PrintInherList()
{
    for (const auto& classs : m_ClassMap)
    {
        std::vector<std::string> vList = GetInheritList(classs.second);
        for (const auto& tmp : vList)
        {
            cout << tmp << "|";
        }
        cout << endl;
    }
}

bool ClassTable::IsSubType(Symbol childType, Symbol parentType, TypeCheckEnvironment& env)
{
    // assert(childType && parentType);
    if (!childType || !parentType)
    {
        return false;
    }

    if (childType == parentType)
    {
        return true;
    }

    if (childType == idtable.lookup_string("SELF_TYPE"))
    {
        childType = env.pCurrentClass->name;
    }

    if (parentType == idtable.lookup_string("SELF_TYPE"))
    {
        parentType = env.pCurrentClass->name;
    }

    const string sChildClassName = string(childType->get_string());
    const string sParentClassName = string(parentType->get_string());
    if (m_ClassMap.find(sChildClassName) == m_ClassMap.end())
    {
        // 没找到
        semant_error();
        cerr << "Not find Type: " << sChildClassName << endl;
    }
    if (m_ClassMap.find(sParentClassName) == m_ClassMap.end())
    {
        semant_error();
        cerr << "Not find Type: " << sParentClassName << endl;
    }

    vector<string> vecChildInheritList = GetInheritList(m_ClassMap[sChildClassName]);
    return std::find(std::begin(vecChildInheritList), std::end(vecChildInheritList), sParentClassName) !=
           std::end(vecChildInheritList);
}

class__class* ClassTable::GetClassByName(const std::string& sClassName)
{
    auto iter = m_ClassMap.find(sClassName);
    return iter == m_ClassMap.end() ? nullptr : iter->second;
}

/*   This is the entry point to the semantic checker.

     Your checker should do the following two things:

     1) Check that the program is semantically correct
     2) Decorate the abstract syntax tree with type information
        by setting the `type' field in each Expression node.
        (see `tree.h')

     You are free to first do 1), make sure you catch all semantic
     errors. Part 2) can be done in a second stage, when you want
     to build mycoolc.
 */
void program_class::semant()
{
    initialize_constants();

    /* ClassTable constructor may do some semantic analysis */
    ClassTable* classtable = new ClassTable(classes);

    /* some semantic analysis code may go here */
    // classtable->PrintInherList();

    TypeCheckEnvironment env;
    env.pClassTable = classtable;
    env.MethodIdTable = classtable->GetAllMethodTable();

    // 遍历classes
    for (int i = classes->first(); classes->more(i); i = classes->next(i))
    {
        class__class* Class = dynamic_cast<class__class*>(classes->nth(i));
        assert(Class);
        env.pCurrentClass = Class;
        Class->ClassTypeCheck(env);
    }

    if (classtable->errors())
    {
        cerr << "Compilation halted due to static semantic errors." << endl;
        exit(1);
    }
}

std::vector<method_class*> class__class::GetAllMethod() const
{
    std::vector<method_class*> result;
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

std::vector<attr_class*> class__class::GetAllAttr() const
{
    std::vector<attr_class*> result;
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
