#!/usr/bin/env lua
--[[
*    typra.ex
*    Copyright (C) 2019  Paul Coelho
*
*    This program is free software: you can redistribute it and/or modify
*    it under the terms of the GNU General Public License as published by
*    the Free Software Foundation, either version 3 of the License, or
*    (at your option) any later version.
*
*    This program is distributed in the hope that it will be useful,
*    but WITHOUT ANY WARRANTY; without even the implied warranty of
*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*    GNU General Public License for more details.
*
*    You should have received a copy of the GNU General Public License
*    along with this program.  If not, see <http://www.gnu.org/licenses/>.
--]]

dofile("type.inc")

--HERE Enums
Enum1 = {
	"Value11",
	"Value12",
	"Value13",
}
Enum2 = {
	"Value21",
	"Value22",
	"Value23",
}
--HERE Structs
Struct1 = {
	{"next","Struct1",{},0},
	{"field1","float",{},{2}},
	{"field2","double",{},{3}},
	{"field3","int",{},{2,2}},
	{"field4","char*",{},{}},
	{"field5","int",{},3},
	{"field6","Enum1",{},{}},
	{"field7","Enum2",{},{}},
	{"field8","int",{["field6"]={["Value11"]=true}},{}},
	{"field9","int",{["field6"]={["Value11"]=true}},{}},
	{"field10","int",{["field6"]={["Value12"]=true}},{}},
	{"field11","int",{["field6"]={["Value12"]=true},["field7"]={["Value21"]=true}},{}},
	{"field12","int",{["field6"]={["Value12"]=true},["field7"]={["Value22"]=true,["Value23"]=true}},{}},
	{"field13","int",{["field6"]={["Value13"]=true}},{}},
	{"field14","int",{},{}},
	{"field15","int",{},"field14"},
	{"field16","Struct2",{},2},
	{"field17","Struct2",{},{2}},
}
Struct2 = {
	{"field1","int",{},{}},
	{"field2","int",{},{}},
}
--HERE
Enums = listHere("Enums")
Structs = listHere("Structs")
Stimulus = {
	{"Enums"},
	{"Structs"},
	{"findString(\"hello ok again and again\",\"hello\")"},
	{"findString(\"hello ok again() and again\",\"again\")"},
	{"findString(\"hello() ok again and again\",\"hello\")"},
	{"Enum1"},
	{"Enum2"},
	{"allOf(Enum1)"},
	{"allBefore(Enum1,\"Value12\")"},
	{"allExcept(Enum1,allBefore)"},
	{"unionSet(allBefore,allExcept)"},
	{"differSet(allOf,allBefore)"},
	{"equalSet(allExcept,differSet)"},
	{"equalSet(allOf,allExcept)"},
	{"allBefore(Enum1,\"Value13\")"},
	{"interSet(differSet,allOf)"},
	{"structTagSpace(Struct1)","structTagSpace(Struct1)"},
	{"equalPlaid(structTagSpace,structTagSpace,structTagSpace)"},
	{"interPlaid({[\"field6\"]=allBefore},structTagSpace,structTagSpace)","structTagSpace"},
	{"interPlaid({[\"field6\"]=allBefore},{[\"field7\"]=allExcept(Enum1,allBefore)},structTagSpace)","structTagSpace"},
	{"nestStruct(chainStruct(Struct1))"},
	{"nestStruct(splitStruct(Struct1))"},
	{"\"Enum1\",Enum1"},
	{"\"Struct1\",Struct1"},
	{"\"Struct1\",Struct1"},
	{"\"Struct1\",Struct1"},
	{"\"Struct1\",Struct1"},
	{"\"Struct1\",Struct1"},
	{"\"Str\""},
	{"\"Enum1\""},
	{"\"Struct1\",Struct1"},
	{"\"Int\",\"int\""},
	{""},
	{""},
	{"\"Enum1\",Enum1"},
	{"\"Struct1\",Struct1"},
	{"\"Struct1\",Struct1"},
	{""},
}
Monitor = {
	"showAny",
	"showAny",
	"showFind",
	"showFind",
	"showFind",
	"showList",
	"showList",
	"showSet",
	"showSet",
	"showSet",
	"showSet",
	"showSet",
	"showBool",
	"showBool",
	"showSet",
	"showSet",
	"showPlaid",
	"showBool",
	"showPlaid",
	"showPlaid",
	"showAny",
	"showAny",
	"showEnumC",
	"showStructC",
	"showReadC",
	"showWriteC",
	"showRandC",
	"showCompC",
	"showAllocC",
	"showAllocC",
	"showAllocC",
	"showAllocC",
	"showTypeH",
	"showTypeC",
	"showEnumHs",
	"showStructHs",
	"showAccessHs",
	"showHelpHs",
}
Expected = {
	"{[\"Enum1\"]={[1]=\"Value11\",[2]=\"Value12\",[3]=\"Value13\"},"..
	"[\"Enum2\"]={[1]=\"Value21\",[2]=\"Value22\",[3]=\"Value23\"}}",
	"{[\"Struct1\"]={[1]={[1]=\"next\",[2]=\"Struct1\",[3]={},[4]=0},"..
	"[2]={[1]=\"field1\",[2]=\"float\",[3]={},[4]={[1]=2}},"..
	"[3]={[1]=\"field2\",[2]=\"double\",[3]={},[4]={[1]=3}},"..
	"[4]={[1]=\"field3\",[2]=\"int\",[3]={},[4]={[1]=2,[2]=2}},"..
	"[5]={[1]=\"field4\",[2]=\"char*\",[3]={},[4]={}},"..
	"[6]={[1]=\"field5\",[2]=\"int\",[3]={},[4]=3},"..
	"[7]={[1]=\"field6\",[2]=\"Enum1\",[3]={},[4]={}},"..
	"[8]={[1]=\"field7\",[2]=\"Enum2\",[3]={},[4]={}},"..
	"[9]={[1]=\"field8\",[2]=\"int\",[3]={[\"field6\"]={[\"Value11\"]=true}},[4]={}},"..
	"[10]={[1]=\"field9\",[2]=\"int\",[3]={[\"field6\"]={[\"Value11\"]=true}},[4]={}},"..
	"[11]={[1]=\"field10\",[2]=\"int\",[3]={[\"field6\"]={[\"Value12\"]=true}},[4]={}},"..
	"[12]={[1]=\"field11\",[2]=\"int\",[3]={[\"field6\"]={[\"Value12\"]=true},[\"field7\"]={[\"Value21\"]=true}},[4]={}},"..
	"[13]={[1]=\"field12\",[2]=\"int\",[3]={[\"field6\"]={[\"Value12\"]=true},[\"field7\"]={[\"Value22\"]=true,[\"Value23\"]=true}},[4]={}},"..
	"[14]={[1]=\"field13\",[2]=\"int\",[3]={[\"field6\"]={[\"Value13\"]=true}},[4]={}},"..
	"[15]={[1]=\"field14\",[2]=\"int\",[3]={},[4]={}},"..
	"[16]={[1]=\"field15\",[2]=\"int\",[3]={},[4]=\"field14\"},"..
	"[17]={[1]=\"field16\",[2]=\"Struct2\",[3]={},[4]=2},"..
	"[18]={[1]=\"field17\",[2]=\"Struct2\",[3]={},[4]={[1]=2}}},"..
	"[\"Struct2\"]={[1]={[1]=\"field1\",[2]=\"int\",[3]={},[4]={}},"..
	"[2]={[1]=\"field2\",[2]=\"int\",[3]={},[4]={}}}}",
	"1,-5",
	"10,5;22,-5",
	"1,5",
	"Value11,Value12,Value13",
	"Value21,Value22,Value23",
	"Value11,Value12,Value13",
	"Value11,(Value12),(Value13)",
	"(Value11),Value12,Value13",
	"Value11,Value12,Value13",
	"(Value11),Value12,Value13",
	"true",
	"false",
	"Value11,Value12,(Value13)",
	"(Value11),Value12,Value13",
	"field6:Value11,Value12,Value13;field7:Value21,Value22,Value23",
	"true",
	"field6:Value11,Value12,(Value13);field7:Value21,Value22,Value23",
	"empty",
	"{[1]={[1]=1,[2]=8},[2]={[1]=9,[2]=10},[3]={[1]=15,[2]=18}}",
	"{[1]={[1]=9,[2]=11},[2]={[1]=12,[2]=14}}",
	"enum Enum1 {\n"..
	"    Value11,\n"..
	"    Value12,\n"..
	"    Value13,\n"..
	"};",
	"struct Struct1 {\n"..
	"    struct {\n"..
	"        struct Struct1* next;\n"..
	"        float field1[2];\n"..
	"        double field2[3];\n"..
	"        int field3[2][2];\n"..
	"        char* field4;\n"..
	"        int* field5;\n"..
	"        enum Enum1 field6;\n"..
	"        enum Enum2 field7;\n"..
	"    };\n"..
	"    union {\n"..
	"        struct {\n"..
	"            int field8;\n"..
	"            int field9;\n"..
	"        };\n"..
	"        int field10;\n"..
	"    };\n"..
	"    union {\n"..
	"        int field11;\n"..
	"        int field12;\n"..
	"        int field13;\n"..
	"    };\n"..
	"    struct {\n"..
	"        int field14;\n"..
	"        int* field15;\n"..
	"        struct Struct2* field16;\n"..
	"        struct Struct2 field17[2];\n"..
	"    };\n"..
	"};",
	"void readStruct1(struct Struct1 *ptr, int idx)\n"..
	"{\n"..
	"    // {[1]=\"next\",[2]=\"Struct1\",[3]={},[4]=0}\n"..
	"    for (int i1 = 0; i1 < 2; i1++)\n"..
	"        {double temp = readNum(idx); ptr->field1[i1] = temp;}\n"..
	"    for (int i1 = 0; i1 < 3; i1++)\n"..
	"        ptr->field2[i1] = readNum(idx);\n"..
	"    for (int i1 = 0; i1 < 2; i1++)\n"..
	"        for (int i2 = 0; i2 < 2; i2++)\n"..
	"            ptr->field3[i1][i2] = readInt(idx);\n"..
	"    {char *temp = readStr(idx); allocStr(&ptr->field4,temp);}\n"..
	"    allocInt(&ptr->field5,3);\n"..
	"    for (int i = 0; i < 3; i++)\n"..
	"        ptr->field5[i] = readInt(idx);\n"..
	"    {int temp = readInt(idx); ptr->field6 = temp;}\n"..
	"    {int temp = readInt(idx); ptr->field7 = temp;}\n"..
	"    if (((ptr->field6==Value11)))\n"..
	"        ptr->field8 = readInt(idx);\n"..
	"    if (((ptr->field6==Value11)))\n"..
	"        ptr->field9 = readInt(idx);\n"..
	"    if (((ptr->field6==Value12)))\n"..
	"        ptr->field10 = readInt(idx);\n"..
	"    if (((ptr->field6==Value12)) and\n"..
	"        ((ptr->field7==Value21)))\n"..
	"        ptr->field11 = readInt(idx);\n"..
	"    if (((ptr->field6==Value12)) and\n"..
	"        ((ptr->field7==Value22) or (ptr->field7==Value23)))\n"..
	"        ptr->field12 = readInt(idx);\n"..
	"    if (((ptr->field6==Value13)))\n"..
	"        ptr->field13 = readInt(idx);\n"..
	"    ptr->field14 = readInt(idx);\n"..
	"    allocInt(&ptr->field15,ptr->field14);\n"..
	"    for (int i = 0; i < ptr->field14; i++)\n"..
	"        ptr->field15[i] = readInt(idx);\n"..
	"    allocStruct2(&ptr->field16,2);\n"..
	"    for (int i = 0; i < 2; i++)\n"..
	"        readStruct2(&ptr->field16[i],idx);\n"..
	"    for (int i1 = 0; i1 < 2; i1++)\n"..
	"        readStruct2(&ptr->field17[i1],idx);\n"..
	"}",
	"void writeStruct1(struct Struct1 *ptr, int idx)\n"..
	"{\n"..
	"    // {[1]=\"next\",[2]=\"Struct1\",[3]={},[4]=0}\n"..
	"    for (int i1 = 0; i1 < 2; i1++)\n"..
	"        {double temp = ptr->field1[i1]; writeNum(temp,idx);}\n"..
	"    for (int i1 = 0; i1 < 3; i1++)\n"..
	"        writeNum(ptr->field2[i1],idx);\n"..
	"    for (int i1 = 0; i1 < 2; i1++)\n"..
	"        for (int i2 = 0; i2 < 2; i2++)\n"..
	"            writeInt(ptr->field3[i1][i2],idx);\n"..
	"    writeStr(ptr->field4,idx);\n"..
	"    for (int i = 0; i < 3; i++)\n"..
	"        writeInt(ptr->field5[i],idx);\n"..
	"    {int temp = ptr->field6; writeInt(temp,idx);}\n"..
	"    {int temp = ptr->field7; writeInt(temp,idx);}\n"..
	"    if (((ptr->field6==Value11)))\n"..
	"        writeInt(ptr->field8,idx);\n"..
	"    if (((ptr->field6==Value11)))\n"..
	"        writeInt(ptr->field9,idx);\n"..
	"    if (((ptr->field6==Value12)))\n"..
	"        writeInt(ptr->field10,idx);\n"..
	"    if (((ptr->field6==Value12)) and\n"..
	"        ((ptr->field7==Value21)))\n"..
	"        writeInt(ptr->field11,idx);\n"..
	"    if (((ptr->field6==Value12)) and\n"..
	"        ((ptr->field7==Value22) or (ptr->field7==Value23)))\n"..
	"        writeInt(ptr->field12,idx);\n"..
	"    if (((ptr->field6==Value13)))\n"..
	"        writeInt(ptr->field13,idx);\n"..
	"    writeInt(ptr->field14,idx);\n"..
	"    for (int i = 0; i < ptr->field14; i++)\n"..
	"        writeInt(ptr->field15[i],idx);\n"..
	"    for (int i = 0; i < 2; i++)\n"..
	"        writeStruct2(&ptr->field16[i],idx);\n"..
	"    for (int i1 = 0; i1 < 2; i1++)\n"..
	"        writeStruct2(&ptr->field17[i1],idx);\n"..
	"}",
	"void randStruct1(struct Struct1 *ptr)\n"..
	"{\n"..
	"    // {[1]=\"next\",[2]=\"Struct1\",[3]={},[4]=0}\n"..
	"    for (int i1 = 0; i1 < 2; i1++)\n"..
	"        ptr->field1[i1] = 0.1;\n"..
	"    for (int i1 = 0; i1 < 3; i1++)\n"..
	"        ptr->field2[i1] = 1.1;\n"..
	"    for (int i1 = 0; i1 < 2; i1++)\n"..
	"        for (int i2 = 0; i2 < 2; i2++)\n"..
	"            ptr->field3[i1][i2] = 0;\n"..
	"    {char *temp = \"hello ok again\"; allocStr(&ptr->field4,temp);}\n"..
	"    allocInt(&ptr->field5,3);\n"..
	"    for (int i = 0; i < 3; i++)\n"..
	"        ptr->field5[i] = 1;\n"..
	"    ptr->field6 = 2;\n"..
	"    ptr->field7 = 3;\n"..
	"    if (((ptr->field6==Value11)))\n"..
	"        ptr->field8 = 4;\n"..
	"    if (((ptr->field6==Value11)))\n"..
	"        ptr->field9 = 5;\n"..
	"    if (((ptr->field6==Value12)))\n"..
	"        ptr->field10 = 6;\n"..
	"    if (((ptr->field6==Value12)) and\n"..
	"        ((ptr->field7==Value21)))\n"..
	"        ptr->field11 = 7;\n"..
	"    if (((ptr->field6==Value12)) and\n"..
	"        ((ptr->field7==Value22) or (ptr->field7==Value23)))\n"..
	"        ptr->field12 = 8;\n"..
	"    if (((ptr->field6==Value13)))\n"..
	"        ptr->field13 = 9;\n"..
	"    ptr->field14 = 10;\n"..
	"    allocInt(&ptr->field15,ptr->field14);\n"..
	"    for (int i = 0; i < ptr->field14; i++)\n"..
	"        ptr->field15[i] = 11;\n"..
	"    allocStruct2(&ptr->field16,2);\n"..
	"    for (int i = 0; i < 2; i++)\n"..
	"        randStruct2(&ptr->field16[i]);\n"..
	"    for (int i1 = 0; i1 < 2; i1++)\n"..
	"        randStruct2(&ptr->field17[i1]);\n"..
	"}",
	"int compStruct1(struct Struct1 *ptr, struct Struct1 *cmp)\n"..
	"{\n"..
	"    // {[1]=\"next\",[2]=\"Struct1\",[3]={},[4]=0}\n"..
	"    for (int i1 = 0; i1 < 2; i1++)\n"..
	"        if (ptr->field1[i1] != cmp->field1[i1]) return false;\n"..
	"    for (int i1 = 0; i1 < 3; i1++)\n"..
	"        if (ptr->field2[i1] != cmp->field2[i1]) return false;\n"..
	"    for (int i1 = 0; i1 < 2; i1++)\n"..
	"        for (int i2 = 0; i2 < 2; i2++)\n"..
	"            if (ptr->field3[i1][i2] != cmp->field3[i1][i2]) return false;\n"..
	"    if (strcmp(ptr->field4,cmp->field4) != 0) return false;\n"..
	"    for (int i = 0; i < 3; i++)\n"..
	"        if (ptr->field5[i] != cmp->field5[i]) return false;\n"..
	"    if (ptr->field6 != cmp->field6) return false;\n"..
	"    if (ptr->field7 != cmp->field7) return false;\n"..
	"    if (((ptr->field6==Value11)))\n"..
	"        if (ptr->field8 != cmp->field8) return false;\n"..
	"    if (((ptr->field6==Value11)))\n"..
	"        if (ptr->field9 != cmp->field9) return false;\n"..
	"    if (((ptr->field6==Value12)))\n"..
	"        if (ptr->field10 != cmp->field10) return false;\n"..
	"    if (((ptr->field6==Value12)) and\n"..
	"        ((ptr->field7==Value21)))\n"..
	"        if (ptr->field11 != cmp->field11) return false;\n"..
	"    if (((ptr->field6==Value12)) and\n"..
	"        ((ptr->field7==Value22) or (ptr->field7==Value23)))\n"..
	"        if (ptr->field12 != cmp->field12) return false;\n"..
	"    if (((ptr->field6==Value13)))\n"..
	"        if (ptr->field13 != cmp->field13) return false;\n"..
	"    if (ptr->field14 != cmp->field14) return false;\n"..
	"    for (int i = 0; i < ptr->field14; i++)\n"..
	"        if (ptr->field15[i] != cmp->field15[i]) return false;\n"..
	"    for (int i = 0; i < 2; i++)\n"..
	"        if (!compStruct2(&ptr->field16[i], &cmp->field16[i])) return false;\n"..
	"    for (int i1 = 0; i1 < 2; i1++)\n"..
	"        if (!compStruct2(&ptr->field17[i1], &cmp->field17[i1])) return false;\n"..
	"    return true;\n"..
	"}",
	"void allocStr(char **ptr, const char *str)\n"..
	"{\n"..
	"    *ptr = realloc(*ptr,strlen(str)+1);\n"..
	"    strcpy(*ptr,str);\n"..
	"}",
	"void allocEnum1(enum Enum1 **ptr, int siz)\n"..
	"{\n"..
	"    *ptr = realloc(*ptr,siz*sizeof(enum Enum1));\n"..
	"}",
	"void allocStruct1(struct Struct1 **ptr, int siz)\n"..
	"{\n"..
	"    *ptr = realloc(*ptr,siz*sizeof(struct Struct1));\n"..
	"    for (int i = 0; i < siz; i++) {\n"..
	"        (*ptr)[i].field5 = 0;\n"..
	"        (*ptr)[i].field15 = 0;\n"..
	"        (*ptr)[i].field16 = 0;}\n"..
	"}",
	"void allocInt(int **ptr, int siz)\n"..
	"{\n"..
	"    *ptr = realloc(*ptr,siz*sizeof(int));\n"..
	"}",
	"enum Enum1 {\n"..
	"    Value11,\n"..
	"    Value12,\n"..
	"    Value13,\n"..
	"};\n"..
	"enum Enum2 {\n"..
	"    Value21,\n"..
	"    Value22,\n"..
	"    Value23,\n"..
	"};\n"..
	"struct Struct1 {\n"..
	"    struct {\n"..
	"        struct Struct1* next;\n"..
	"        float field1[2];\n"..
	"        double field2[3];\n"..
	"        int field3[2][2];\n"..
	"        char* field4;\n"..
	"        int* field5;\n"..
	"        enum Enum1 field6;\n"..
	"        enum Enum2 field7;\n"..
	"    };\n"..
	"    union {\n"..
	"        struct {\n"..
	"            int field8;\n"..
	"            int field9;\n"..
	"        };\n"..
	"        int field10;\n"..
	"    };\n"..
	"    union {\n"..
	"        int field11;\n"..
	"        int field12;\n"..
	"        int field13;\n"..
	"    };\n"..
	"    struct {\n"..
	"        int field14;\n"..
	"        int* field15;\n"..
	"        struct Struct2* field16;\n"..
	"        struct Struct2 field17[2];\n"..
	"    };\n"..
	"};\n"..
	"struct Struct2 {\n"..
	"    struct {\n"..
	"        int field1;\n"..
	"        int field2;\n"..
	"    };\n"..
	"};\n"..
	"void allocInt(int **ptr, int siz);\n"..
	"void allocDouble(double **ptr, int siz);\n"..
	"void allocFloat(float **ptr, int siz);\n"..
	"void allocStr(char **ptr, const char *str);\n"..
	"void allocStruct1(struct Struct1 **ptr, int siz);\n"..
	"void allocStruct2(struct Struct2 **ptr, int siz);\n"..
	"void readStruct1(struct Struct1 *ptr, int idx);\n"..
	"void readStruct2(struct Struct2 *ptr, int idx);\n"..
	"void writeStruct1(struct Struct1 *ptr, int idx);\n"..
	"void writeStruct2(struct Struct2 *ptr, int idx);\n"..
	"void randStruct1(struct Struct1 *ptr);\n"..
	"void randStruct2(struct Struct2 *ptr);\n"..
	"int compStruct1(struct Struct1 *ptr, struct Struct1 *cmp);\n"..
	"int compStruct2(struct Struct2 *ptr, struct Struct2 *cmp);",
	("#include <stdlib.h>\n"..
	"#include <string.h>\n"..
	"#include \"face.h\"\n"..
	"void allocInt(int **ptr, int siz)\n"..
	"{\n"..
	"    *ptr = realloc(*ptr,siz*sizeof(int));\n"..
	"}\n"..
	"void allocDouble(double **ptr, int siz)\n"..
	"{\n"..
	"    *ptr = realloc(*ptr,siz*sizeof(double));\n"..
	"}\n"..
	"void allocFloat(float **ptr, int siz)\n"..
	"{\n"..
	"    *ptr = realloc(*ptr,siz*sizeof(float));\n"..
	"}\n"..
	"void allocStr(char **ptr, const char *str)\n"..
	"{\n"..
	"    *ptr = realloc(*ptr,strlen(str)+1);\n"..
	"    strcpy(*ptr,str);\n"..
	"}\n"..
	"void allocStruct1(struct Struct1 **ptr, int siz)\n"..
	"{\n"..
	"    *ptr = realloc(*ptr,siz*sizeof(struct Struct1));\n"..
	"    for (int i = 0; i < siz; i++) {\n"..
	"        (*ptr)[i].field5 = 0;\n"..
	"        (*ptr)[i].field15 = 0;\n"..
	"        (*ptr)[i].field16 = 0;}\n"..
	"}\n"..
	"void allocStruct2(struct Struct2 **ptr, int siz)\n"..
	"{\n"..
	"    *ptr = realloc(*ptr,siz*sizeof(struct Struct2));\n"..
	"    for (int i = 0; i < siz; i++) {}\n"..
	"}\n")..
	("void readStruct1(struct Struct1 *ptr, int idx)\n"..
	"{\n"..
	"    // {[1]=\"next\",[2]=\"Struct1\",[3]={},[4]=0}\n"..
	"    for (int i1 = 0; i1 < 2; i1++)\n"..
	"        {double temp = readNum(idx); ptr->field1[i1] = temp;}\n"..
	"    for (int i1 = 0; i1 < 3; i1++)\n"..
	"        ptr->field2[i1] = readNum(idx);\n"..
	"    for (int i1 = 0; i1 < 2; i1++)\n"..
	"        for (int i2 = 0; i2 < 2; i2++)\n"..
	"            ptr->field3[i1][i2] = readInt(idx);\n"..
	"    {char *temp = readStr(idx); allocStr(&ptr->field4,temp);}\n"..
	"    allocInt(&ptr->field5,3);\n"..
	"    for (int i = 0; i < 3; i++)\n"..
	"        ptr->field5[i] = readInt(idx);\n"..
	"    {int temp = readInt(idx); ptr->field6 = temp;}\n"..
	"    {int temp = readInt(idx); ptr->field7 = temp;}\n"..
	"    if (((ptr->field6==Value11)))\n"..
	"        ptr->field8 = readInt(idx);\n"..
	"    if (((ptr->field6==Value11)))\n"..
	"        ptr->field9 = readInt(idx);\n"..
	"    if (((ptr->field6==Value12)))\n"..
	"        ptr->field10 = readInt(idx);\n"..
	"    if (((ptr->field6==Value12)) and\n"..
	"        ((ptr->field7==Value21)))\n"..
	"        ptr->field11 = readInt(idx);\n"..
	"    if (((ptr->field6==Value12)) and\n"..
	"        ((ptr->field7==Value22) or (ptr->field7==Value23)))\n"..
	"        ptr->field12 = readInt(idx);\n"..
	"    if (((ptr->field6==Value13)))\n"..
	"        ptr->field13 = readInt(idx);\n"..
	"    ptr->field14 = readInt(idx);\n"..
	"    allocInt(&ptr->field15,ptr->field14);\n"..
	"    for (int i = 0; i < ptr->field14; i++)\n"..
	"        ptr->field15[i] = readInt(idx);\n"..
	"    allocStruct2(&ptr->field16,2);\n"..
	"    for (int i = 0; i < 2; i++)\n"..
	"        readStruct2(&ptr->field16[i],idx);\n"..
	"    for (int i1 = 0; i1 < 2; i1++)\n"..
	"        readStruct2(&ptr->field17[i1],idx);\n"..
	"}\n"..
	"void readStruct2(struct Struct2 *ptr, int idx)\n"..
	"{\n"..
	"    ptr->field1 = readInt(idx);\n"..
	"    ptr->field2 = readInt(idx);\n"..
	"}\n")..
	("void writeStruct1(struct Struct1 *ptr, int idx)\n"..
	"{\n"..
	"    // {[1]=\"next\",[2]=\"Struct1\",[3]={},[4]=0}\n"..
	"    for (int i1 = 0; i1 < 2; i1++)\n"..
	"        {double temp = ptr->field1[i1]; writeNum(temp,idx);}\n"..
	"    for (int i1 = 0; i1 < 3; i1++)\n"..
	"        writeNum(ptr->field2[i1],idx);\n"..
	"    for (int i1 = 0; i1 < 2; i1++)\n"..
	"        for (int i2 = 0; i2 < 2; i2++)\n"..
	"            writeInt(ptr->field3[i1][i2],idx);\n"..
	"    writeStr(ptr->field4,idx);\n"..
	"    for (int i = 0; i < 3; i++)\n"..
	"        writeInt(ptr->field5[i],idx);\n"..
	"    {int temp = ptr->field6; writeInt(temp,idx);}\n"..
	"    {int temp = ptr->field7; writeInt(temp,idx);}\n"..
	"    if (((ptr->field6==Value11)))\n"..
	"        writeInt(ptr->field8,idx);\n"..
	"    if (((ptr->field6==Value11)))\n"..
	"        writeInt(ptr->field9,idx);\n"..
	"    if (((ptr->field6==Value12)))\n"..
	"        writeInt(ptr->field10,idx);\n"..
	"    if (((ptr->field6==Value12)) and\n"..
	"        ((ptr->field7==Value21)))\n"..
	"        writeInt(ptr->field11,idx);\n"..
	"    if (((ptr->field6==Value12)) and\n"..
	"        ((ptr->field7==Value22) or (ptr->field7==Value23)))\n"..
	"        writeInt(ptr->field12,idx);\n"..
	"    if (((ptr->field6==Value13)))\n"..
	"        writeInt(ptr->field13,idx);\n"..
	"    writeInt(ptr->field14,idx);\n"..
	"    for (int i = 0; i < ptr->field14; i++)\n"..
	"        writeInt(ptr->field15[i],idx);\n"..
	"    for (int i = 0; i < 2; i++)\n"..
	"        writeStruct2(&ptr->field16[i],idx);\n"..
	"    for (int i1 = 0; i1 < 2; i1++)\n"..
	"        writeStruct2(&ptr->field17[i1],idx);\n"..
	"}\n"..
	"void writeStruct2(struct Struct2 *ptr, int idx)\n"..
	"{\n"..
	"    writeInt(ptr->field1,idx);\n"..
	"    writeInt(ptr->field2,idx);\n"..
	"}\n")..
	("void randStruct1(struct Struct1 *ptr)\n"..
	"{\n"..
	"    // {[1]=\"next\",[2]=\"Struct1\",[3]={},[4]=0}\n"..
	"    for (int i1 = 0; i1 < 2; i1++)\n"..
	"        ptr->field1[i1] = 2.1;\n"..
	"    for (int i1 = 0; i1 < 3; i1++)\n"..
	"        ptr->field2[i1] = 3.1;\n"..
	"    for (int i1 = 0; i1 < 2; i1++)\n"..
	"        for (int i2 = 0; i2 < 2; i2++)\n"..
	"            ptr->field3[i1][i2] = 26;\n"..
	"    {char *temp = \"hello ok again\"; allocStr(&ptr->field4,temp);}\n"..
	"    allocInt(&ptr->field5,3);\n"..
	"    for (int i = 0; i < 3; i++)\n"..
	"        ptr->field5[i] = 27;\n"..
	"    ptr->field6 = 28;\n"..
	"    ptr->field7 = 29;\n"..
	"    if (((ptr->field6==Value11)))\n"..
	"        ptr->field8 = 30;\n"..
	"    if (((ptr->field6==Value11)))\n"..
	"        ptr->field9 = 31;\n"..
	"    if (((ptr->field6==Value12)))\n"..
	"        ptr->field10 = 32;\n"..
	"    if (((ptr->field6==Value12)) and\n"..
	"        ((ptr->field7==Value21)))\n"..
	"        ptr->field11 = 33;\n"..
	"    if (((ptr->field6==Value12)) and\n"..
	"        ((ptr->field7==Value22) or (ptr->field7==Value23)))\n"..
	"        ptr->field12 = 34;\n"..
	"    if (((ptr->field6==Value13)))\n"..
	"        ptr->field13 = 35;\n"..
	"    ptr->field14 = 36;\n"..
	"    allocInt(&ptr->field15,ptr->field14);\n"..
	"    for (int i = 0; i < ptr->field14; i++)\n"..
	"        ptr->field15[i] = 37;\n"..
	"    allocStruct2(&ptr->field16,2);\n"..
	"    for (int i = 0; i < 2; i++)\n"..
	"        randStruct2(&ptr->field16[i]);\n"..
	"    for (int i1 = 0; i1 < 2; i1++)\n"..
	"        randStruct2(&ptr->field17[i1]);\n"..
	"}\n"..
	"void randStruct2(struct Struct2 *ptr)\n"..
	"{\n"..
	"    ptr->field1 = 38;\n"..
	"    ptr->field2 = 39;\n"..
	"}\n")..
	("int compStruct1(struct Struct1 *ptr, struct Struct1 *cmp)\n"..
	"{\n"..
	"    // {[1]=\"next\",[2]=\"Struct1\",[3]={},[4]=0}\n"..
	"    for (int i1 = 0; i1 < 2; i1++)\n"..
	"        if (ptr->field1[i1] != cmp->field1[i1]) return false;\n"..
	"    for (int i1 = 0; i1 < 3; i1++)\n"..
	"        if (ptr->field2[i1] != cmp->field2[i1]) return false;\n"..
	"    for (int i1 = 0; i1 < 2; i1++)\n"..
	"        for (int i2 = 0; i2 < 2; i2++)\n"..
	"            if (ptr->field3[i1][i2] != cmp->field3[i1][i2]) return false;\n"..
	"    if (strcmp(ptr->field4,cmp->field4) != 0) return false;\n"..
	"    for (int i = 0; i < 3; i++)\n"..
	"        if (ptr->field5[i] != cmp->field5[i]) return false;\n"..
	"    if (ptr->field6 != cmp->field6) return false;\n"..
	"    if (ptr->field7 != cmp->field7) return false;\n"..
	"    if (((ptr->field6==Value11)))\n"..
	"        if (ptr->field8 != cmp->field8) return false;\n"..
	"    if (((ptr->field6==Value11)))\n"..
	"        if (ptr->field9 != cmp->field9) return false;\n"..
	"    if (((ptr->field6==Value12)))\n"..
	"        if (ptr->field10 != cmp->field10) return false;\n"..
	"    if (((ptr->field6==Value12)) and\n"..
	"        ((ptr->field7==Value21)))\n"..
	"        if (ptr->field11 != cmp->field11) return false;\n"..
	"    if (((ptr->field6==Value12)) and\n"..
	"        ((ptr->field7==Value22) or (ptr->field7==Value23)))\n"..
	"        if (ptr->field12 != cmp->field12) return false;\n"..
	"    if (((ptr->field6==Value13)))\n"..
	"        if (ptr->field13 != cmp->field13) return false;\n"..
	"    if (ptr->field14 != cmp->field14) return false;\n"..
	"    for (int i = 0; i < ptr->field14; i++)\n"..
	"        if (ptr->field15[i] != cmp->field15[i]) return false;\n"..
	"    for (int i = 0; i < 2; i++)\n"..
	"        if (!compStruct2(&ptr->field16[i], &cmp->field16[i])) return false;\n"..
	"    for (int i1 = 0; i1 < 2; i1++)\n"..
	"        if (!compStruct2(&ptr->field17[i1], &cmp->field17[i1])) return false;\n"..
	"    return true;\n"..
	"}\n"..
	"int compStruct2(struct Struct2 *ptr, struct Struct2 *cmp)\n"..
	"{\n"..
	"    if (ptr->field1 != cmp->field1) return false;\n"..
	"    if (ptr->field2 != cmp->field2) return false;\n"..
	"    return true;\n"..
	"}"),
	"data Enum1 =\n"..
	"    Value11 |\n"..
	"    Value12 |\n"..
	"    Value13 |\n"..
	"    Enum1s deriving (Eq)\n"..
	"--",
	"data Struct1A1X8 = Struct1A1X8\n"..
	"    [Struct1] -- next\n"..
	"    [Float] -- field1\n"..
	"    [Double] -- field2\n"..
	"    [Int] -- field3\n"..
	"    String -- field4\n"..
	"    [Int] -- field5\n"..
	"    Enum1 -- field6\n"..
	"    Enum2 -- field7\n"..
	"    deriving (Eq)\n"..
	"data Struct1A9X10 = Struct1A9X10\n"..
	"    Int -- field8\n"..
	"    Int -- field9\n"..
	"    deriving (Eq)\n"..
	"data Struct1A15X18 = Struct1A15X18\n"..
	"    Int -- field14\n"..
	"    [Int] -- field15\n"..
	"    [Struct2] -- field16\n"..
	"    [Struct2] -- field17\n"..
	"    deriving (Eq)\n"..
	"data Struct1A9X11 =\n"..
	"    Struct1A9X11B9X10 Struct1A9X10 |\n"..
	"    Struct1A9X11B11 Int | -- field10\n"..
	"    Struct1A9X11Bs deriving (Eq)\n"..
	"data Struct1A12X14 =\n"..
	"    Struct1A12X14B12 Int | -- field11\n"..
	"    Struct1A12X14B13 Int | -- field12\n"..
	"    Struct1A12X14B14 Int | -- field13\n"..
	"    Struct1A12X14Bs deriving (Eq)\n"..
	"data Struct1 = Struct1\n"..
	"    Struct1A1X8\n"..
	"    Struct1A9X11\n"..
	"    Struct1A12X14\n"..
	"    Struct1A15X18\n"..
	"    deriving (Eq)\n"..
	"--",
	("getStruct1Cnext :: Struct1 -> [Struct1]\n"..
	"getStruct1Cnext (Struct1 (Struct1A1X8 a a2 a3 a4 a5 a6 a7 a8) a9x11 a12x14 a15x18) = a\n"..
	"getStruct1Cfield1 :: Struct1 -> [Float]\n"..
	"getStruct1Cfield1 (Struct1 (Struct1A1X8 a1 a a3 a4 a5 a6 a7 a8) a9x11 a12x14 a15x18) = a\n"..
	"getStruct1Cfield2 :: Struct1 -> [Double]\n"..
	"getStruct1Cfield2 (Struct1 (Struct1A1X8 a1 a2 a a4 a5 a6 a7 a8) a9x11 a12x14 a15x18) = a\n"..
	"getStruct1Cfield3 :: Struct1 -> [Int]\n"..
	"getStruct1Cfield3 (Struct1 (Struct1A1X8 a1 a2 a3 a a5 a6 a7 a8) a9x11 a12x14 a15x18) = a\n"..
	"getStruct1Cfield4 :: Struct1 -> String\n"..
	"getStruct1Cfield4 (Struct1 (Struct1A1X8 a1 a2 a3 a4 a a6 a7 a8) a9x11 a12x14 a15x18) = a\n"..
	"getStruct1Cfield5 :: Struct1 -> [Int]\n"..
	"getStruct1Cfield5 (Struct1 (Struct1A1X8 a1 a2 a3 a4 a5 a a7 a8) a9x11 a12x14 a15x18) = a\n"..
	"getStruct1Cfield6 :: Struct1 -> Enum1\n"..
	"getStruct1Cfield6 (Struct1 (Struct1A1X8 a1 a2 a3 a4 a5 a6 a a8) a9x11 a12x14 a15x18) = a\n"..
	"getStruct1Cfield7 :: Struct1 -> Enum2\n"..
	"getStruct1Cfield7 (Struct1 (Struct1A1X8 a1 a2 a3 a4 a5 a6 a7 a) a9x11 a12x14 a15x18) = a\n"..
	"getStruct1Cfield8 :: Struct1 -> Int\n"..
	"getStruct1Cfield8 (Struct1 a1x8 (Struct1A9X11B9X10 (Struct1A9X10 a a10)) a12x14 a15x18) = a\n"..
	"getStruct1Cfield9 :: Struct1 -> Int\n"..
	"getStruct1Cfield9 (Struct1 a1x8 (Struct1A9X11B9X10 (Struct1A9X10 a9 a)) a12x14 a15x18) = a\n"..
	"getStruct1Cfield10 :: Struct1 -> Int\n"..
	"getStruct1Cfield10 (Struct1 a1x8 (Struct1A9X11B11 a) a12x14 a15x18) = a\n"..
	"getStruct1Cfield11 :: Struct1 -> Int\n"..
	"getStruct1Cfield11 (Struct1 a1x8 a9x11 (Struct1A12X14B12 a) a15x18) = a\n"..
	"getStruct1Cfield12 :: Struct1 -> Int\n"..
	"getStruct1Cfield12 (Struct1 a1x8 a9x11 (Struct1A12X14B13 a) a15x18) = a\n"..
	"getStruct1Cfield13 :: Struct1 -> Int\n"..
	"getStruct1Cfield13 (Struct1 a1x8 a9x11 (Struct1A12X14B14 a) a15x18) = a\n"..
	"getStruct1Cfield14 :: Struct1 -> Int\n"..
	"getStruct1Cfield14 (Struct1 a1x8 a9x11 a12x14 (Struct1A15X18 a a16 a17 a18)) = a\n"..
	"getStruct1Cfield15 :: Struct1 -> [Int]\n"..
	"getStruct1Cfield15 (Struct1 a1x8 a9x11 a12x14 (Struct1A15X18 a15 a a17 a18)) = a\n"..
	"getStruct1Cfield16 :: Struct1 -> [Struct2]\n"..
	"getStruct1Cfield16 (Struct1 a1x8 a9x11 a12x14 (Struct1A15X18 a15 a16 a a18)) = a\n"..
	"getStruct1Cfield17 :: Struct1 -> [Struct2]\n"..
	"getStruct1Cfield17 (Struct1 a1x8 a9x11 a12x14 (Struct1A15X18 a15 a16 a17 a)) = a\n"..
	"--\n")..
	("setStruct1Cnext :: Struct1 -> [Struct1] -> Struct1\n"..
	"setStruct1Cnext (Struct1 (Struct1A1X8 _ a2 a3 a4 a5 a6 a7 a8) a9x11 a12x14 a15x18) a = (Struct1 (Struct1A1X8 a a2 a3 a4 a5 a6 a7 a8) a9x11 a12x14 a15x18)\n"..
	"setStruct1Cfield1 :: Struct1 -> [Float] -> Struct1\n"..
	"setStruct1Cfield1 (Struct1 (Struct1A1X8 a1 _ a3 a4 a5 a6 a7 a8) a9x11 a12x14 a15x18) a = (Struct1 (Struct1A1X8 a1 a a3 a4 a5 a6 a7 a8) a9x11 a12x14 a15x18)\n"..
	"setStruct1Cfield2 :: Struct1 -> [Double] -> Struct1\n"..
	"setStruct1Cfield2 (Struct1 (Struct1A1X8 a1 a2 _ a4 a5 a6 a7 a8) a9x11 a12x14 a15x18) a = (Struct1 (Struct1A1X8 a1 a2 a a4 a5 a6 a7 a8) a9x11 a12x14 a15x18)\n"..
	"setStruct1Cfield3 :: Struct1 -> [Int] -> Struct1\n"..
	"setStruct1Cfield3 (Struct1 (Struct1A1X8 a1 a2 a3 _ a5 a6 a7 a8) a9x11 a12x14 a15x18) a = (Struct1 (Struct1A1X8 a1 a2 a3 a a5 a6 a7 a8) a9x11 a12x14 a15x18)\n"..
	"setStruct1Cfield4 :: Struct1 -> String -> Struct1\n"..
	"setStruct1Cfield4 (Struct1 (Struct1A1X8 a1 a2 a3 a4 _ a6 a7 a8) a9x11 a12x14 a15x18) a = (Struct1 (Struct1A1X8 a1 a2 a3 a4 a a6 a7 a8) a9x11 a12x14 a15x18)\n"..
	"setStruct1Cfield5 :: Struct1 -> [Int] -> Struct1\n"..
	"setStruct1Cfield5 (Struct1 (Struct1A1X8 a1 a2 a3 a4 a5 _ a7 a8) a9x11 a12x14 a15x18) a = (Struct1 (Struct1A1X8 a1 a2 a3 a4 a5 a a7 a8) a9x11 a12x14 a15x18)\n"..
	"setStruct1Cfield6 :: Struct1 -> Enum1 -> Struct1\n"..
	"setStruct1Cfield6 (Struct1 (Struct1A1X8 a1 a2 a3 a4 a5 a6 _ a8) a9x11 a12x14 a15x18) a = (Struct1 (Struct1A1X8 a1 a2 a3 a4 a5 a6 a a8) a9x11 a12x14 a15x18)\n"..
	"setStruct1Cfield7 :: Struct1 -> Enum2 -> Struct1\n"..
	"setStruct1Cfield7 (Struct1 (Struct1A1X8 a1 a2 a3 a4 a5 a6 a7 _) a9x11 a12x14 a15x18) a = (Struct1 (Struct1A1X8 a1 a2 a3 a4 a5 a6 a7 a) a9x11 a12x14 a15x18)\n"..
	"setStruct1Cfield8 :: Struct1 -> Int -> Struct1\n"..
	"setStruct1Cfield8 (Struct1 a1x8 (Struct1A9X11B9X10 (Struct1A9X10 _ a10)) a12x14 a15x18) a = (Struct1 a1x8 (Struct1A9X11B9X10 (Struct1A9X10 a a10)) a12x14 a15x18)\n"..
	"setStruct1Cfield9 :: Struct1 -> Int -> Struct1\n"..
	"setStruct1Cfield9 (Struct1 a1x8 (Struct1A9X11B9X10 (Struct1A9X10 a9 _)) a12x14 a15x18) a = (Struct1 a1x8 (Struct1A9X11B9X10 (Struct1A9X10 a9 a)) a12x14 a15x18)\n"..
	"setStruct1Cfield10 :: Struct1 -> Int -> Struct1\n"..
	"setStruct1Cfield10 (Struct1 a1x8 (Struct1A9X11B11 _) a12x14 a15x18) a = (Struct1 a1x8 (Struct1A9X11B11 a) a12x14 a15x18)\n"..
	"setStruct1Cfield11 :: Struct1 -> Int -> Struct1\n"..
	"setStruct1Cfield11 (Struct1 a1x8 a9x11 (Struct1A12X14B12 _) a15x18) a = (Struct1 a1x8 a9x11 (Struct1A12X14B12 a) a15x18)\n"..
	"setStruct1Cfield12 :: Struct1 -> Int -> Struct1\n"..
	"setStruct1Cfield12 (Struct1 a1x8 a9x11 (Struct1A12X14B13 _) a15x18) a = (Struct1 a1x8 a9x11 (Struct1A12X14B13 a) a15x18)\n"..
	"setStruct1Cfield13 :: Struct1 -> Int -> Struct1\n"..
	"setStruct1Cfield13 (Struct1 a1x8 a9x11 (Struct1A12X14B14 _) a15x18) a = (Struct1 a1x8 a9x11 (Struct1A12X14B14 a) a15x18)\n"..
	"setStruct1Cfield14 :: Struct1 -> Int -> Struct1\n"..
	"setStruct1Cfield14 (Struct1 a1x8 a9x11 a12x14 (Struct1A15X18 _ a16 a17 a18)) a = (Struct1 a1x8 a9x11 a12x14 (Struct1A15X18 a a16 a17 a18))\n"..
	"setStruct1Cfield15 :: Struct1 -> [Int] -> Struct1\n"..
	"setStruct1Cfield15 (Struct1 a1x8 a9x11 a12x14 (Struct1A15X18 a15 _ a17 a18)) a = (Struct1 a1x8 a9x11 a12x14 (Struct1A15X18 a15 a a17 a18))\n"..
	"setStruct1Cfield16 :: Struct1 -> [Struct2] -> Struct1\n"..
	"setStruct1Cfield16 (Struct1 a1x8 a9x11 a12x14 (Struct1A15X18 a15 a16 _ a18)) a = (Struct1 a1x8 a9x11 a12x14 (Struct1A15X18 a15 a16 a a18))\n"..
	"setStruct1Cfield17 :: Struct1 -> [Struct2] -> Struct1\n"..
	"setStruct1Cfield17 (Struct1 a1x8 a9x11 a12x14 (Struct1A15X18 a15 a16 a17 _)) a = (Struct1 a1x8 a9x11 a12x14 (Struct1A15X18 a15 a16 a17 a))\n"..
	"--"),
	"listHelp :: Int -> IO a -> IO [a]\n"..
	"listHelp 0 b = return []\n"..
	"listHelp a b = do\n"..
	"    d <- b\n"..
	"    e <- listHelp (a-1) b\n"..
	"    return (d:e)\n"..
	"floatHelp :: IO Double -> IO Float\n"..
	"floatHelp = fmap doubleToFloatcondHelp :: Bool -> a -> IO a -> IO a\n"..
	"condHelp True _ a = a\n"..
	"condHelp False a _ = return a\n"..
	"firstHelp :: a -> [a] -> IO a\n"..
	"firstHelp :: a [] = return a\n"..
	"firstHelp :: a (b:c)\n"..
	"    | a == b = firstHelp a c\n"..
	"    | otherwise = return b\n"..
	"--",
}
function linesOf(str)
	local result = {}
	local todo = str
	while (todo ~= "") do
		local line = ""
		while (todo ~= "") and (string.sub(todo,1,1) ~= "\n") do
			line = line..string.sub(todo,1,1)
			todo = string.sub(todo,2,-1)
		end
		if (todo ~= "") then
			todo = string.sub(todo,2,-1)
		end
		result[#result+1] = line
	end
	return result
end
file = io.open("type.txt","w")
io.output(file)
abbrev = {}
for k,v in ipairs(Stimulus) do
	chunk = "return "..Monitor[k].."("
	for j,u in ipairs(v) do
		expand = u
		for key,val in pairs(abbrev) do
			found = findString(expand,key)
			delta = 0
			for index,value in ipairs(found) do
				pos = value[1]
				len = value[2]
				if len < 0 then
					neg = -len
					sum = pos+delta
					if sum > 1 then before = string.sub(expand,1,sum-1) else before = "" end
					if sum+neg <= string.len(expand) then after = string.sub(expand,sum+neg,-1) else after = "" end
					expand = before..val..after
					delta = delta + val:len()-neg
				end
			end
		end
		if j > 1 then chunk = chunk.."," end
		chunk = chunk..expand
	end
	chunk = chunk..")"
	--print(chunk)
	func = assert(load(chunk))
	actual = func()
	io.write(v[1].." "..actual.."\n")
	found = {string.find(v[1],"^%a+%(")}
	if found and found[1] and found[2] then
		pos = found[1]
		lim = found[2]
		key = string.sub(v[1],pos,lim-1)
		abbrev[key] = expand
	end
end
io.close(file)
file = io.open("type.txt","r")
io.input(file)
for k,v in ipairs(Expected) do
	for key,val in ipairs(linesOf(Stimulus[k][1].." "..v)) do
		line = io.read();
		if line ~= val then print("error1: "..line.." "..val); os.exit() end
		--print(line)
	end
end
line = io.read(); if line ~= nil then print("error2: "..line); os.exit() end
io.close(file)
file = io.open("typer.h", "w")
file:write(showTypeH().."\n")
file:close()
file = io.open("typer.c", "w")
file:write(showTyperC().."\n")
file:close()
file = io.open("typer.hs", "w")
file:write(showTyperHs().."\n")
file:close()
file = io.open("typer.lua", "w")
file:write(showTyperLua().."\n")
file:close()
print(showAny(arg))
