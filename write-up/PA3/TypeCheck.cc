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
#include <set>

using std::vector;
using std::string;

void class__class::ClassTypeCheck(TypeCheckEnvironment& env)
{
    env.ObjIdTable.enterscope();

    env.ObjIdTable.addid("self", idtable.lookup_string("SELF_TYPE"));

    std::vector<attr_class*> vecAttr = GetAllAttr();
    for (const auto pAttr : vecAttr)
    {
        pAttr->FeatureTypeCheck(env);
    }

    vector<method_class*> vecMethod = GetAllMethod();
    for (const auto pMethod : vecMethod)
    {
        pMethod->FeatureTypeCheck(env);
    }

    env.ObjIdTable.exitscope();
}

void attr_class::FeatureTypeCheck(TypeCheckEnvironment& env)
{
    Symbol initType = init->ExpTypeCheck(env);

    if (initType)
    {
        if (!env.pClassTable->IsSubType(initType, type_decl))
        {
            env.pClassTable->semant_error(env.pCurrentClass);
            cerr << "Type not match" << endl;
        }
    }
    const string sAttrName = std::string(name->get_string());
    if (env.ObjIdTable.probe(sAttrName))
    {
        env.pClassTable->semant_error(env.pCurrentClass);
        cerr << "Attr is exist in this scope. : " << sAttrName << endl;
    }
    else
    {
        env.ObjIdTable.addid(sAttrName, type_decl);
    }
}

void method_class::FeatureTypeCheck(TypeCheckEnvironment& env)
{
    env.ObjIdTable.enterscope();
    for (int i = formals->first(); formals->more(i); i = formals->next(i))
    {
        formal_class* pFormal = dynamic_cast<formal_class*>(formals->nth(i));
        assert(pFormal);
        const string sFormalName = pFormal->name->get_string();
        if (env.ObjIdTable.probe(sFormalName))
        {
            env.pClassTable->semant_error(env.pCurrentClass);
            cerr << "Formal is exist in this scope: " << sFormalName << endl;
        }
        else
        {
            env.ObjIdTable.addid(sFormalName, pFormal->type_decl);
        }
    }

    Symbol expType = expr->ExpTypeCheck(env);

    if (!env.pClassTable->IsSubType(expType, return_type))
    {
        env.pClassTable->semant_error(env.pCurrentClass);
        cerr << "Type Not Match" << endl;
    }

    env.ObjIdTable.exitscope();
}

Symbol assign_class::ExpTypeCheck(TypeCheckEnvironment& env)
{
    const string sObjName = name->get_string();
    Symbol objType = env.ObjIdTable.lookup(sObjName);
    if (!objType)
    {
        env.pClassTable->semant_error(env.pCurrentClass);
        cerr << "not find obj: " << sObjName << endl;
    }
    else
    {
        Symbol expType = expr->ExpTypeCheck(env);

        if (!env.pClassTable->IsSubType(expType, objType))
        {
            env.pClassTable->semant_error(env.pCurrentClass);
            cerr << "Type Not Match" << endl;
        }
        set_type(objType);
    }

    return get_type();
}

Symbol static_dispatch_class::ExpTypeCheck(TypeCheckEnvironment& env)
{
    return get_type();
}

// ±éÀú¼Ì³ÐÊ÷,Ñ°ÕÒMethod
method_class* GetMethodByEnv(TypeCheckEnvironment& env, class__class* pFindClass, const string& sMethodName)
{
    // class__class* pFindClass = env.pClassTable->GetClassByName(sFindClass);
    assert(pFindClass);
    vector<string> vecInheritList = env.pClassTable->GetInheritList(pFindClass);
    for (const auto inherit : vecInheritList)
    {
        auto iter = env.MethodIdTable.find(inherit);
        assert(iter != env.MethodIdTable.end());

        auto iterMethod = iter->second.find(sMethodName);
        if (iterMethod == iter->second.end())
        {
            continue;
        }
        else
        {
            return iterMethod->second;
        }
    }
    return nullptr;
}

Symbol dispatch_class::ExpTypeCheck(TypeCheckEnvironment& env)
{
    Symbol expType = expr->ExpTypeCheck(env);
    if (!expType)
    {
        env.pClassTable->semant_error(env.pCurrentClass);
        cerr << "Can't Get expType " << endl;
        return nullptr;
    }

    class__class* pExpClass = env.pClassTable->GetClassByName(expType->get_string());
    assert(pExpClass);

    const string sMethondName = name->get_string();
    method_class* pMethod = GetMethodByEnv(env, pExpClass, sMethondName);

    if (!pMethod)
    {
        env.pClassTable->semant_error(env.pCurrentClass);
        cerr << "Can't find Method: " << sMethondName << endl;
    }
    else
    {
        vector<Symbol> vecActualType;
        for (int i = actual->first(); actual->more(i); i = actual->next(i))
        {
            Expression exp = actual->nth(i);
            Symbol expType = exp->ExpTypeCheck(env);
            vecActualType.push_back(expType);
        }

        vector<Symbol> vecFormalType;
        for (int i = pMethod->formals->first(); pMethod->formals->more(i); i = pMethod->formals->next(i))
        {
            formal_class* pFormal = dynamic_cast<formal_class*>(pMethod->formals->nth(i));
            vecFormalType.push_back(pFormal->type_decl);
        }

        if (vecActualType.size() != vecFormalType.size())
        {
            env.pClassTable->semant_error(env.pCurrentClass);
            cerr << "Param Num not Same" << endl;
        }
        else
        {
            for (size_t i = 0; i < vecActualType.size(); ++i)
            {
                Symbol actType = vecActualType[i];
                Symbol formalType = vecFormalType[i];
                if (!env.pClassTable->IsSubType(actType, formalType))
                {
                    env.pClassTable->semant_error(env.pCurrentClass);
                    cerr << "Type not Match" << endl;
                }
            }

            if (pMethod->return_type == idtable.lookup_string("SELF_TYPE"))
            {
                set_type(expType);
            }
            else
            {
                set_type(pMethod->return_type);
            }
        }
    }

    return get_type();
}

Symbol cond_class::ExpTypeCheck(TypeCheckEnvironment& env)
{
    Symbol predType = pred->ExpTypeCheck(env);
    Symbol thenType = then_exp->ExpTypeCheck(env);
    Symbol elseType = else_exp->ExpTypeCheck(env);
    if (predType != idtable.lookup_string("Bool"))
    {
        env.pClassTable->semant_error(env.pCurrentClass);
        cerr << "predType Shoud be bool" << endl;
    }
    Symbol pAncestorType = env.pClassTable->GetLeastCommonAncestor(thenType, elseType);
    set_type(pAncestorType);
    return get_type();
}

Symbol loop_class::ExpTypeCheck(TypeCheckEnvironment& env)
{
    Symbol predType = pred->ExpTypeCheck(env);
    Symbol bodyType = body->ExpTypeCheck(env);

    if (predType != idtable.lookup_string("Bool"))
    {
        env.pClassTable->semant_error(env.pCurrentClass);
        cerr << "predType Should be Bool" << endl;
    }
    set_type(bodyType);
    return get_type();
}

Symbol typcase_class::ExpTypeCheck(TypeCheckEnvironment& env)
{
    Symbol expType = expr->ExpTypeCheck(env);

    std::set<string> setCaseType;
    Symbol resultType = nullptr;
    for (int i = cases->first(); cases->more(i); i = cases->next(i))
    {
        branch_class* pBranch = dynamic_cast<branch_class*>(cases->nth(i));
        assert(pBranch);

        const string sCaseType = pBranch->type_decl->get_string();
        if (setCaseType.find(sCaseType) != setCaseType.end())
        {
            env.pClassTable->semant_error(env.pCurrentClass);
            cerr << "Duplicate branch " << sCaseType << " in case statement." << endl;
        }
        setCaseType.insert(sCaseType);

        env.ObjIdTable.enterscope();

        env.ObjIdTable.addid(pBranch->name->get_string(), pBranch->type_decl);
        Symbol expType = expr->ExpTypeCheck(env);
        if (!resultType)
        {
            resultType = expType;
        }
        else
        {
            resultType = env.pClassTable->GetLeastCommonAncestor(resultType, expType);
        }

        env.ObjIdTable.exitscope();
    }
    set_type(resultType);

    return get_type();
}

Symbol block_class::ExpTypeCheck(TypeCheckEnvironment& env)
{
    for (int i = body->first(); body->more(i); i = body->next(i))
    {
        Expression exp = body->nth(i);
        set_type(exp->ExpTypeCheck(env));
    }
    return get_type();
}

Symbol let_class::ExpTypeCheck(TypeCheckEnvironment& env)
{
    Symbol initType = init->ExpTypeCheck(env);
    if (!env.pClassTable->IsSubType(initType, type_decl))
    {
        env.pClassTable->semant_error(env.pCurrentClass);
        cerr << "Type Error" << endl;
    }
    env.ObjIdTable.enterscope();
    env.ObjIdTable.addid(identifier->get_string(), type_decl);

    Symbol bodyType = body->ExpTypeCheck(env);
    set_type(bodyType);
    env.ObjIdTable.exitscope();

    return get_type();
}

Symbol plus_class::ExpTypeCheck(TypeCheckEnvironment& env)
{
    Symbol e1Type = e1->ExpTypeCheck(env);
    Symbol e2Type = e2->ExpTypeCheck(env);

    if (e1Type != idtable.lookup_string("Int"))
    {
        env.pClassTable->semant_error(env.pCurrentClass);
        cerr << "Plus exp need Int" << endl;
    }
    if (e2Type != e1Type)
    {
        env.pClassTable->semant_error(env.pCurrentClass);
        cerr << "e2 should same as e1" << endl;
    }

    set_type(e1Type);

    return get_type();
}

Symbol sub_class::ExpTypeCheck(TypeCheckEnvironment& env)
{
    Symbol e1Type = e1->ExpTypeCheck(env);
    Symbol e2Type = e2->ExpTypeCheck(env);

    if (e1Type != idtable.lookup_string("Int"))
    {
        env.pClassTable->semant_error(env.pCurrentClass);
        cerr << "Sub exp need Int" << endl;
    }
    if (e2Type != e1Type)
    {
        env.pClassTable->semant_error(env.pCurrentClass);
        cerr << "e2 should same as e1" << endl;
    }

    set_type(e1Type);
    return get_type();
}

Symbol mul_class::ExpTypeCheck(TypeCheckEnvironment& env)
{
    Symbol e1Type = e1->ExpTypeCheck(env);
    Symbol e2Type = e2->ExpTypeCheck(env);

    if (e1Type != idtable.lookup_string("Int"))
    {
        env.pClassTable->semant_error(env.pCurrentClass);
        cerr << "mul exp need Int" << endl;
    }
    if (e2Type != e1Type)
    {
        env.pClassTable->semant_error(env.pCurrentClass);
        cerr << "e2 should same as e1" << endl;
    }

    set_type(e1Type);
    return get_type();
}

Symbol divide_class::ExpTypeCheck(TypeCheckEnvironment& env)
{
    Symbol e1Type = e1->ExpTypeCheck(env);
    Symbol e2Type = e2->ExpTypeCheck(env);

    if (e1Type != idtable.lookup_string("Int"))
    {
        env.pClassTable->semant_error(env.pCurrentClass);
        cerr << "divide exp need Int" << endl;
    }
    if (e2Type != e1Type)
    {
        env.pClassTable->semant_error(env.pCurrentClass);
        cerr << "e2 should same as e1" << endl;
    }

    set_type(e1Type);
    return get_type();
}

Symbol neg_class::ExpTypeCheck(TypeCheckEnvironment& env)
{
    Symbol e1Type = e1->ExpTypeCheck(env);
    if (e1Type != idtable.lookup_string("Int"))
    {
        env.pClassTable->semant_error(env.pCurrentClass);
        cerr << "should be Int" << endl;
    }
    set_type(idtable.lookup_string("Int"));
    return get_type();
}

Symbol lt_class::ExpTypeCheck(TypeCheckEnvironment& env)
{
    Symbol e1Type = e1->ExpTypeCheck(env);
    Symbol e2Type = e2->ExpTypeCheck(env);

    if (e1Type != idtable.lookup_string("Int"))
    {
        env.pClassTable->semant_error(env.pCurrentClass);
        cerr << "divide exp need Int" << endl;
    }
    if (e2Type != e1Type)
    {
        env.pClassTable->semant_error(env.pCurrentClass);
        cerr << "e2 should same as e1" << endl;
    }

    set_type(idtable.lookup_string("Bool"));
    return get_type();
}

Symbol eq_class::ExpTypeCheck(TypeCheckEnvironment& env)
{
    Symbol e1Type = e1->ExpTypeCheck(env);
    Symbol e2Type = e2->ExpTypeCheck(env);

    if (e1Type != e2Type)
    {
        env.pClassTable->semant_error(env.pCurrentClass);
        cerr << "e2 should same as e1" << endl;
    }

    set_type(idtable.lookup_string("Bool"));
    return get_type();
}

Symbol leq_class::ExpTypeCheck(TypeCheckEnvironment& env)
{
    Symbol e1Type = e1->ExpTypeCheck(env);
    Symbol e2Type = e2->ExpTypeCheck(env);

    if (e1Type != idtable.lookup_string("Int"))
    {
        env.pClassTable->semant_error(env.pCurrentClass);
        cerr << "divide exp need Int" << endl;
    }
    if (e2Type != e1Type)
    {
        env.pClassTable->semant_error(env.pCurrentClass);
        cerr << "e2 should same as e1" << endl;
    }

    set_type(idtable.lookup_string("Bool"));
    return get_type();
}

Symbol comp_class::ExpTypeCheck(TypeCheckEnvironment& env)
{
    Symbol e1Type = e1->ExpTypeCheck(env);
    if (e1Type != idtable.lookup_string("Bool"))
    {
        env.pClassTable->semant_error(env.pCurrentClass);
        cerr << "should be Bool" << endl;
    }
    set_type(idtable.lookup_string("Bool"));
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
    Symbol pStr = idtable.lookup_string("String");
    set_type(pStr);
    return get_type();
}

Symbol new__class::ExpTypeCheck(TypeCheckEnvironment& env)
{
    const string sClassName = type_name->get_string();
    class__class* pClass = env.pClassTable->GetClassByName(sClassName);
    if (pClass)
    {
        set_type(pClass->name);
    }
    return get_type();
}

Symbol isvoid_class::ExpTypeCheck(TypeCheckEnvironment& env)
{
    Symbol e1Type = e1->ExpTypeCheck(env);
    set_type(idtable.lookup_string("Bool"));
    return get_type();
}

Symbol no_expr_class::ExpTypeCheck(TypeCheckEnvironment& env)
{
    return get_type();
}

Symbol object_class::ExpTypeCheck(TypeCheckEnvironment& env)
{
    Symbol pObj = env.ObjIdTable.lookup(name->get_string());
    set_type(pObj);
    if (pObj == idtable.lookup_string("SELF_TYPE"))
    {
        return env.pCurrentClass->name;
    }
    return get_type();
}