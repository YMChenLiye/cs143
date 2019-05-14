#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include "semant.h"
#include "utilities.h"
#include <assert.h>
#include <iostream>
#include <map>
#include <string>
#include <vector>
#include "cool-tree.h"
#include "stringtab.h"
#include "symtab.h"
#include "list.h"

void class__class::ClassTypeCheck(TypeCheckEnvironment& env)
{
    cout << "Start Check Class: " << name->get_string() << endl;
    env.ObjIdTable.enterscope();
    std::vector<attr_class*> vecAttr = GetAllAttr();
    for (const auto pAttr : vecAttr)
    {
        cout << "Check Attr: " << pAttr->name->get_string() << endl;
        cout << "Attr Type = " << pAttr->type_decl->get_string() << endl;
        // void* initType = pAttr->init->ExpTypeCheck(env);
        Symbol initType = pAttr->init->ExpTypeCheck(env);
        if (initType != pAttr->type_decl)
        {
            cerr << "Type Not Same " << endl;
        }

        env.ObjIdTable.addid(std::string(pAttr->name->get_string()), pAttr);
    }

    env.ObjIdTable.exitscope();
    cout << "Finish Check Class: " << name->get_string() << endl;
}

void attr_class::FeatureTypeCheck(TypeCheckEnvironment& env)
{
}

void method_class::FeatureTypeCheck(TypeCheckEnvironment& env)
{
}

Symbol assign_class::ExpTypeCheck(TypeCheckEnvironment& env)
{
    if (type)
        return type;
    return get_type();
}

Symbol static_dispatch_class::ExpTypeCheck(TypeCheckEnvironment& env)
{
    return get_type();
}

Symbol dispatch_class::ExpTypeCheck(TypeCheckEnvironment& env)
{
    return get_type();
}

Symbol cond_class::ExpTypeCheck(TypeCheckEnvironment& env)
{
    return get_type();
}

Symbol loop_class::ExpTypeCheck(TypeCheckEnvironment& env)
{
    return get_type();
}

Symbol typcase_class::ExpTypeCheck(TypeCheckEnvironment& env)
{
    return get_type();
}

Symbol block_class::ExpTypeCheck(TypeCheckEnvironment& env)
{
    return get_type();
}

Symbol let_class::ExpTypeCheck(TypeCheckEnvironment& env)
{
    return get_type();
}

Symbol plus_class::ExpTypeCheck(TypeCheckEnvironment& env)
{
    return get_type();
}

Symbol sub_class::ExpTypeCheck(TypeCheckEnvironment& env)
{
    return get_type();
}

Symbol mul_class::ExpTypeCheck(TypeCheckEnvironment& env)
{
    return get_type();
}

Symbol divide_class::ExpTypeCheck(TypeCheckEnvironment& env)
{
    return get_type();
}

Symbol neg_class::ExpTypeCheck(TypeCheckEnvironment& env)
{
    return get_type();
}

Symbol lt_class::ExpTypeCheck(TypeCheckEnvironment& env)
{
    return get_type();
}

Symbol eq_class::ExpTypeCheck(TypeCheckEnvironment& env)
{
    return get_type();
}

Symbol leq_class::ExpTypeCheck(TypeCheckEnvironment& env)
{
    return get_type();
}

Symbol comp_class::ExpTypeCheck(TypeCheckEnvironment& env)
{
    return get_type();
}

Symbol int_const_class::ExpTypeCheck(TypeCheckEnvironment& env)
{
    Symbol pInt = idtable.lookup_string("Int");
    set_type(pInt);
    return get_type();
}

Symbol bool_const_class::ExpTypeCheck(TypeCheckEnvironment& env)
{
    Symbol pBool = idtable.lookup_string("Bool");
    set_type(pBool);
    return get_type();
}

Symbol string_const_class::ExpTypeCheck(TypeCheckEnvironment& env)
{
    return get_type();
}

Symbol new__class::ExpTypeCheck(TypeCheckEnvironment& env)
{
    return get_type();
}

Symbol isvoid_class::ExpTypeCheck(TypeCheckEnvironment& env)
{
    return get_type();
}

Symbol no_expr_class::ExpTypeCheck(TypeCheckEnvironment& env)
{
    return get_type();
}

Symbol object_class::ExpTypeCheck(TypeCheckEnvironment& env)
{
    return get_type();
}