dofile("typra.src")

Stimulus = {
	{"Enumz"},
	{"Structz"},
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
	{"\"Enum1\""},
	{"\"Struct1\",Struct1"},
	{"\"Struct1\",Struct1"},
	{"\"Struct1\",Struct1"},
	{"\"Enum1\",Enum1"},
	{"\"Struct1\",Struct1"},
	{"\"Struct1\",Struct1"},
	{""},
	{"Struct1,{[\"field6\"]={[\"Value12\"]=true},[\"field7\"]={[\"Value21\"]=true,[\"Value22\"]=true}}"},
	{"Struct1,{[\"field7\"]={[\"Value21\"]=true,[\"Value22\"]=true}}"},
	{"Struct1,{[\"field6\"]={[\"Value12\"]=true}}"},
	{"Struct1,{}"},
	{"\"Struct1\",Struct1"},
	{"\"Enum1\",Enum1"},
	{"\"Struct1\",Struct1"},
	{"\"Enum1\",Enum1"},
	{"\"Struct1\",Struct1"},
	{"\"Struct1\",Struct1"},
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
	"showShowSC",
	"showHideSC",
	"showEnumHs",
	"showStructHs",
	"showAccessHs",
	"showHelpHs",
	"showCondHs",
	"showCondHs",
	"showCondHs",
	"showCondHs",
	"showReadHs",
	"showCodeHs",
	"showWriteHs",
	"showCodeLua",
	"showReadLua",
	"showWriteLua",
}
Expected = {
	"{[\"Enum1\"]={[1]=\"Value11\",[2]=\"Value12\",[3]=\"Value13\"},"..
	"[\"Enum2\"]={[1]=\"Value21\",[2]=\"Value22\",[3]=\"Value23\"}}",
	"{[\"Struct1\"]={[1]={[1]=\"next\",[2]=\"Struct1\",[3]={},[4]=0},"..
	"[2]={[1]=\"field1\",[2]=\"Old\",[3]={},[4]={[1]=2}},"..
	"[3]={[1]=\"field2\",[2]=\"Num\",[3]={},[4]={[1]=3}},"..
	"[4]={[1]=\"field3\",[2]=\"Int\",[3]={},[4]={[1]=2,[2]=2}},"..
	"[5]={[1]=\"field4\",[2]=\"Str\",[3]={},[4]={}},"..
	"[6]={[1]=\"field5\",[2]=\"Int\",[3]={},[4]=3},"..
	"[7]={[1]=\"field6\",[2]=\"Enum1\",[3]={},[4]={}},"..
	"[8]={[1]=\"field7\",[2]=\"Enum2\",[3]={},[4]={}},"..
	"[9]={[1]=\"field8\",[2]=\"Int\",[3]={[\"field6\"]={[\"Value11\"]=true}},[4]={}},"..
	"[10]={[1]=\"field9\",[2]=\"Int\",[3]={[\"field6\"]={[\"Value11\"]=true}},[4]={}},"..
	"[11]={[1]=\"field10\",[2]=\"Int\",[3]={[\"field6\"]={[\"Value12\"]=true}},[4]={}},"..
	"[12]={[1]=\"field11\",[2]=\"Int\",[3]={[\"field6\"]={[\"Value12\"]=true},[\"field7\"]={[\"Value21\"]=true}},[4]={}},"..
	"[13]={[1]=\"field12\",[2]=\"Int\",[3]={[\"field6\"]={[\"Value12\"]=true},[\"field7\"]={[\"Value22\"]=true,[\"Value23\"]=true}},[4]={}},"..
	"[14]={[1]=\"field13\",[2]=\"Int\",[3]={[\"field6\"]={[\"Value13\"]=true}},[4]={}},"..
	"[15]={[1]=\"field14\",[2]=\"Int\",[3]={},[4]={}},"..
	"[16]={[1]=\"field15\",[2]=\"Int\",[3]={},[4]=\"field14\"},"..
	"[17]={[1]=\"field16\",[2]=\"Struct2\",[3]={},[4]=2},"..
	"[18]={[1]=\"field17\",[2]=\"Struct2\",[3]={},[4]={[1]=2}}},"..
	"[\"Struct2\"]={[1]={[1]=\"field1\",[2]=\"Int\",[3]={},[4]={}},"..
	"[2]={[1]=\"field2\",[2]=\"Int\",[3]={},[4]={}}}}",
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
	"    Enum1s\n"..
	"};",
	"struct Struct1 {\n"..
	"    // struct { //\n"..
	"        struct Struct1* next;\n"..
	"        float field1[2];\n"..
	"        double field2[3];\n"..
	"        int field3[2][2];\n"..
	"        char* field4;\n"..
	"        int* field5;\n"..
	"        enum Enum1 field6;\n"..
	"        enum Enum2 field7;\n"..
	"    // };\n"..
	"    union {\n"..
	"        struct { // field6:Value11\n"..
	"            int field8;\n"..
	"            int field9;\n"..
	"        };\n"..
	"        int field10; // field6:Value12\n"..
	"    };\n"..
	"    union {\n"..
	"        int field11; // field6:Value12;field7:Value21\n"..
	"        int field12; // field6:Value12;field7:Value22,Value23\n"..
	"        int field13; // field6:Value13\n"..
	"    };\n"..
	"    // struct { //\n"..
	"        int field14;\n"..
	"        int* field15;\n"..
	"        struct Struct2* field16;\n"..
	"        struct Struct2 field17[2];\n"..
	"    // };\n"..
	"};",
	"void readStruct1(struct Struct1 *ptr, int idx)\n"..
	"{\n"..
	"    freeStruct1(ptr);\n"..
	"    allocStruct1(&ptr->next,0);\n"..
	"    for (int i = 0; i < 0; i++)\n"..
	"        readStruct1(&ptr->next[i],idx);\n"..
	"    for (int i1 = 0; i1 < 2; i1++)\n"..
	"        ptr->field1[i1] = readOld(idx);\n"..
	"    for (int i1 = 0; i1 < 3; i1++)\n"..
	"        ptr->field2[i1] = readNum(idx);\n"..
	"    for (int i1 = 0; i1 < 2; i1++)\n"..
	"        for (int i2 = 0; i2 < 2; i2++)\n"..
	"            ptr->field3[i1][i2] = readInt(idx);\n"..
	"    readStr(callStr,&ptr->field4,idx);\n"..
	"    allocInt(&ptr->field5,3);\n"..
	"    for (int i = 0; i < 3; i++)\n"..
	"        ptr->field5[i] = readInt(idx);\n"..
	"    {int temp = readInt(idx); ptr->field6 = temp;}\n"..
	"    {int temp = readInt(idx); ptr->field7 = temp;}\n"..
	"    if (ptr->field6 == Value11) {\n"..
	"        ptr->field8 = readInt(idx);}\n"..
	"    if (ptr->field6 == Value11) {\n"..
	"        ptr->field9 = readInt(idx);}\n"..
	"    if (ptr->field6 == Value12) {\n"..
	"        ptr->field10 = readInt(idx);}\n"..
	"    if ((ptr->field6 == Value12) && (ptr->field7 == Value21)) {\n"..
	"        ptr->field11 = readInt(idx);}\n"..
	"    if ((ptr->field6 == Value12) && ((ptr->field7 == Value22) || (ptr->field7 == Value23))) {\n"..
	"        ptr->field12 = readInt(idx);}\n"..
	"    if (ptr->field6 == Value13) {\n"..
	"        ptr->field13 = readInt(idx);}\n"..
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
	"    for (int i = 0; i < 0; i++)\n"..
	"        writeStruct1(&ptr->next[i],idx);\n"..
	"    for (int i1 = 0; i1 < 2; i1++)\n"..
	"        writeOld(ptr->field1[i1],idx);\n"..
	"    for (int i1 = 0; i1 < 3; i1++)\n"..
	"        writeNum(ptr->field2[i1],idx);\n"..
	"    for (int i1 = 0; i1 < 2; i1++)\n"..
	"        for (int i2 = 0; i2 < 2; i2++)\n"..
	"            writeInt(ptr->field3[i1][i2],idx);\n"..
	"    writeStr(ptr->field4,1,idx);\n"..
	"    for (int i = 0; i < 3; i++)\n"..
	"        writeInt(ptr->field5[i],idx);\n"..
	"    {int temp = ptr->field6; writeInt(temp,idx);}\n"..
	"    {int temp = ptr->field7; writeInt(temp,idx);}\n"..
	"    if (ptr->field6 == Value11) {\n"..
	"        writeInt(ptr->field8,idx);}\n"..
	"    if (ptr->field6 == Value11) {\n"..
	"        writeInt(ptr->field9,idx);}\n"..
	"    if (ptr->field6 == Value12) {\n"..
	"        writeInt(ptr->field10,idx);}\n"..
	"    if ((ptr->field6 == Value12) && (ptr->field7 == Value21)) {\n"..
	"        writeInt(ptr->field11,idx);}\n"..
	"    if ((ptr->field6 == Value12) && ((ptr->field7 == Value22) || (ptr->field7 == Value23))) {\n"..
	"        writeInt(ptr->field12,idx);}\n"..
	"    if (ptr->field6 == Value13) {\n"..
	"        writeInt(ptr->field13,idx);}\n"..
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
	"    freeStruct1(ptr);\n"..
	"    allocStruct1(&ptr->next,0);\n"..
	"    for (int i = 0; i < 0; i++)\n"..
	"        randStruct1(&ptr->next[i]);\n"..
	"    for (int i1 = 0; i1 < 2; i1++)\n"..
	"        ptr->field1[i1] = 0.2;\n"..
	"    for (int i1 = 0; i1 < 3; i1++)\n"..
	"        ptr->field2[i1] = 0.1;\n"..
	"    for (int i1 = 0; i1 < 2; i1++)\n"..
	"        for (int i2 = 0; i2 < 2; i2++)\n"..
	"            ptr->field3[i1][i2] = 10;\n"..
	"    {const char *temp = \"hello ok again\"; assignStr(&ptr->field4,temp);}\n"..
	"    allocInt(&ptr->field5,3);\n"..
	"    for (int i = 0; i < 3; i++)\n"..
	"        ptr->field5[i] = 11;\n"..
	"    ptr->field6 = 12%Enum1s;\n"..
	"    ptr->field7 = 13%Enum2s;\n"..
	"    if (ptr->field6 == Value11) {\n"..
	"        ptr->field8 = 14;}\n"..
	"    if (ptr->field6 == Value11) {\n"..
	"        ptr->field9 = 15;}\n"..
	"    if (ptr->field6 == Value12) {\n"..
	"        ptr->field10 = 16;}\n"..
	"    if ((ptr->field6 == Value12) && (ptr->field7 == Value21)) {\n"..
	"        ptr->field11 = 17;}\n"..
	"    if ((ptr->field6 == Value12) && ((ptr->field7 == Value22) || (ptr->field7 == Value23))) {\n"..
	"        ptr->field12 = 18;}\n"..
	"    if (ptr->field6 == Value13) {\n"..
	"        ptr->field13 = 19;}\n"..
	"    ptr->field14 = 20;\n"..
	"    allocInt(&ptr->field15,ptr->field14);\n"..
	"    for (int i = 0; i < ptr->field14; i++)\n"..
	"        ptr->field15[i] = 21;\n"..
	"    allocStruct2(&ptr->field16,2);\n"..
	"    for (int i = 0; i < 2; i++)\n"..
	"        randStruct2(&ptr->field16[i]);\n"..
	"    for (int i1 = 0; i1 < 2; i1++)\n"..
	"        randStruct2(&ptr->field17[i1]);\n"..
	"}",
	"int compStruct1(struct Struct1 *ptr, struct Struct1 *cmp)\n"..
	"{\n"..
	"    for (int i = 0; i < 0; i++)\n"..
	"        if (!compStruct1(&ptr->next[i], &cmp->next[i])) return 0;\n"..
	"    for (int i1 = 0; i1 < 2; i1++)\n"..
	"        if (ptr->field1[i1] != cmp->field1[i1]) return 0;\n"..
	"    for (int i1 = 0; i1 < 3; i1++)\n"..
	"        if (ptr->field2[i1] != cmp->field2[i1]) return 0;\n"..
	"    for (int i1 = 0; i1 < 2; i1++)\n"..
	"        for (int i2 = 0; i2 < 2; i2++)\n"..
	"            if (ptr->field3[i1][i2] != cmp->field3[i1][i2]) return 0;\n"..
	"    if (strcmp(ptr->field4,cmp->field4) != 0) return 0;\n"..
	"    for (int i = 0; i < 3; i++)\n"..
	"        if (ptr->field5[i] != cmp->field5[i]) return 0;\n"..
	"    if (ptr->field6 != cmp->field6) return 0;\n"..
	"    if (ptr->field7 != cmp->field7) return 0;\n"..
	"    if (ptr->field6 == Value11) {\n"..
	"        if (ptr->field8 != cmp->field8) return 0;}\n"..
	"    if (ptr->field6 == Value11) {\n"..
	"        if (ptr->field9 != cmp->field9) return 0;}\n"..
	"    if (ptr->field6 == Value12) {\n"..
	"        if (ptr->field10 != cmp->field10) return 0;}\n"..
	"    if ((ptr->field6 == Value12) && (ptr->field7 == Value21)) {\n"..
	"        if (ptr->field11 != cmp->field11) return 0;}\n"..
	"    if ((ptr->field6 == Value12) && ((ptr->field7 == Value22) || (ptr->field7 == Value23))) {\n"..
	"        if (ptr->field12 != cmp->field12) return 0;}\n"..
	"    if (ptr->field6 == Value13) {\n"..
	"        if (ptr->field13 != cmp->field13) return 0;}\n"..
	"    if (ptr->field14 != cmp->field14) return 0;\n"..
	"    for (int i = 0; i < ptr->field14; i++)\n"..
	"        if (ptr->field15[i] != cmp->field15[i]) return 0;\n"..
	"    for (int i = 0; i < 2; i++)\n"..
	"        if (!compStruct2(&ptr->field16[i], &cmp->field16[i])) return 0;\n"..
	"    for (int i1 = 0; i1 < 2; i1++)\n"..
	"        if (!compStruct2(&ptr->field17[i1], &cmp->field17[i1])) return 0;\n"..
	"    return 1;\n"..
	"}",
	"void allocEnum1(enum Enum1 **ptr, int siz)\n"..
	"{\n"..
	"    if (*ptr && siz == 0) {free(*ptr); *ptr = 0;}\n"..
	"    if (siz == 0) return;\n"..
	"    allocMem((void**)ptr,siz*sizeof(enum Enum1));\n"..
	"    if (*ptr == 0) ERROR();\n"..
	"    for (int i = 0; i < siz; i++) (*ptr)[i] = 0;\n"..
	"}",
	"void allocStruct1(struct Struct1 **ptr, int siz)\n"..
	"{\n"..
	"    if (*ptr && siz == 0) {free(*ptr); *ptr = 0;}\n"..
	"    if (siz == 0) return;\n"..
	"    allocMem((void**)ptr,siz*sizeof(struct Struct1));\n"..
	"    if (*ptr == 0) ERROR();\n"..
	"    struct Struct1 init = {0};\n"..
	"    for (int i = 0; i < siz; i++)\n"..
	"        memcpy(&(*ptr)[i],&init,sizeof(init));\n"..
	"}",
	"void showStruct1(struct Struct1 *ptr, char **str, int *len)\n"..
	"{\n"..
	"    showOpen(\"Struct1\",str,len);\n"..
	"    for (int i = 0; i < 0; i++)\n"..
	"        {showField(\"next\",str,len,1,i); showStruct1(&ptr->next[i],str,len);}\n"..
	"    for (int i1 = 0; i1 < 2; i1++)\n"..
	"        {showField(\"field1\",str,len,1,i1); showOld(ptr->field1[i1],str,len);}\n"..
	"    for (int i1 = 0; i1 < 3; i1++)\n"..
	"        {showField(\"field2\",str,len,1,i1); showNum(ptr->field2[i1],str,len);}\n"..
	"    for (int i1 = 0; i1 < 2; i1++)\n"..
	"        for (int i2 = 0; i2 < 2; i2++)\n"..
	"            {showField(\"field3\",str,len,2,i1,i2); showInt(ptr->field3[i1][i2],str,len);}\n"..
	"    {showField(\"field4\",str,len,0); showStr(ptr->field4,str,len);}\n"..
	"    for (int i = 0; i < 3; i++)\n"..
	"        {showField(\"field5\",str,len,1,i); showInt(ptr->field5[i],str,len);}\n"..
	"    {showField(\"field6\",str,len,0); showEnum1(ptr->field6,str,len);}\n"..
	"    {showField(\"field7\",str,len,0); showEnum2(ptr->field7,str,len);}\n"..
	"    if (ptr->field6 == Value11) {\n"..
	"        {showField(\"field8\",str,len,0); showInt(ptr->field8,str,len);}}\n"..
	"    if (ptr->field6 == Value11) {\n"..
	"        {showField(\"field9\",str,len,0); showInt(ptr->field9,str,len);}}\n"..
	"    if (ptr->field6 == Value12) {\n"..
	"        {showField(\"field10\",str,len,0); showInt(ptr->field10,str,len);}}\n"..
	"    if ((ptr->field6 == Value12) && (ptr->field7 == Value21)) {\n"..
	"        {showField(\"field11\",str,len,0); showInt(ptr->field11,str,len);}}\n"..
	"    if ((ptr->field6 == Value12) && ((ptr->field7 == Value22) || (ptr->field7 == Value23))) {\n"..
	"        {showField(\"field12\",str,len,0); showInt(ptr->field12,str,len);}}\n"..
	"    if (ptr->field6 == Value13) {\n"..
	"        {showField(\"field13\",str,len,0); showInt(ptr->field13,str,len);}}\n"..
	"    {showField(\"field14\",str,len,0); showInt(ptr->field14,str,len);}\n"..
	"    for (int i = 0; i < ptr->field14; i++)\n"..
	"        {showField(\"field15\",str,len,1,i); showInt(ptr->field15[i],str,len);}\n"..
	"    for (int i = 0; i < 2; i++)\n"..
	"        {showField(\"field16\",str,len,1,i); showStruct2(&ptr->field16[i],str,len);}\n"..
	"    for (int i1 = 0; i1 < 2; i1++)\n"..
	"        {showField(\"field17\",str,len,1,i1); showStruct2(&ptr->field17[i1],str,len);}\n"..
	"    showClose(str,len);\n"..
	"}",
	"int hideStruct1(struct Struct1 *ptr, const char *str, int *len)\n"..
	"{\n"..
	"    allocMark();\n"..
	"    if (!hideOpen(\"Struct1\",str,len)) {allocDrop(); return 0;}\n"..
	"    allocStruct1(&ptr->next,0);\n"..
	"    for (int i = 0; i < 0; i++)\n"..
	"        if (!hideField(\"next\",str,len,1,i) || !hideStruct1(&ptr->next[i],str,len)) {allocDrop(); return 0;}\n"..
	"    for (int i1 = 0; i1 < 2; i1++)\n"..
	"        if (!hideField(\"field1\",str,len,1,i1) || !hideOld(&ptr->field1[i1],str,len)) {allocDrop(); return 0;}\n"..
	"    for (int i1 = 0; i1 < 3; i1++)\n"..
	"        if (!hideField(\"field2\",str,len,1,i1) || !hideNum(&ptr->field2[i1],str,len)) {allocDrop(); return 0;}\n"..
	"    for (int i1 = 0; i1 < 2; i1++)\n"..
	"        for (int i2 = 0; i2 < 2; i2++)\n"..
	"            if (!hideField(\"field3\",str,len,2,i1,i2) || !hideInt(&ptr->field3[i1][i2],str,len)) {allocDrop(); return 0;}\n"..
	"    if (!hideField(\"field4\",str,len,0) || !hideStr(&ptr->field4,str,len)) {allocDrop(); return 0;}\n"..
	"    allocInt(&ptr->field5,3);\n"..
	"    for (int i = 0; i < 3; i++)\n"..
	"        if (!hideField(\"field5\",str,len,1,i) || !hideInt(&ptr->field5[i],str,len)) {allocDrop(); return 0;}\n"..
	"    if (!hideField(\"field6\",str,len,0) || !hideEnum1(&ptr->field6,str,len)) {allocDrop(); return 0;}\n"..
	"    if (!hideField(\"field7\",str,len,0) || !hideEnum2(&ptr->field7,str,len)) {allocDrop(); return 0;}\n"..
	"    if (ptr->field6 == Value11) {\n"..
	"        if (!hideField(\"field8\",str,len,0) || !hideInt(&ptr->field8,str,len)) {allocDrop(); return 0;}}\n"..
	"    if (ptr->field6 == Value11) {\n"..
	"        if (!hideField(\"field9\",str,len,0) || !hideInt(&ptr->field9,str,len)) {allocDrop(); return 0;}}\n"..
	"    if (ptr->field6 == Value12) {\n"..
	"        if (!hideField(\"field10\",str,len,0) || !hideInt(&ptr->field10,str,len)) {allocDrop(); return 0;}}\n"..
	"    if ((ptr->field6 == Value12) && (ptr->field7 == Value21)) {\n"..
	"        if (!hideField(\"field11\",str,len,0) || !hideInt(&ptr->field11,str,len)) {allocDrop(); return 0;}}\n"..
	"    if ((ptr->field6 == Value12) && ((ptr->field7 == Value22) || (ptr->field7 == Value23))) {\n"..
	"        if (!hideField(\"field12\",str,len,0) || !hideInt(&ptr->field12,str,len)) {allocDrop(); return 0;}}\n"..
	"    if (ptr->field6 == Value13) {\n"..
	"        if (!hideField(\"field13\",str,len,0) || !hideInt(&ptr->field13,str,len)) {allocDrop(); return 0;}}\n"..
	"    if (!hideField(\"field14\",str,len,0) || !hideInt(&ptr->field14,str,len)) {allocDrop(); return 0;}\n"..
	"    allocInt(&ptr->field15,ptr->field14);\n"..
	"    for (int i = 0; i < ptr->field14; i++)\n"..
	"        if (!hideField(\"field15\",str,len,1,i) || !hideInt(&ptr->field15[i],str,len)) {allocDrop(); return 0;}\n"..
	"    allocStruct2(&ptr->field16,2);\n"..
	"    for (int i = 0; i < 2; i++)\n"..
	"        if (!hideField(\"field16\",str,len,1,i) || !hideStruct2(&ptr->field16[i],str,len)) {allocDrop(); return 0;}\n"..
	"    for (int i1 = 0; i1 < 2; i1++)\n"..
	"        if (!hideField(\"field17\",str,len,1,i1) || !hideStruct2(&ptr->field17[i1],str,len)) {allocDrop(); return 0;}\n"..
	"    if (!hideClose(str,len)) {allocDrop(); return 0;}\n"..
	"    allocKeep();\n"..
	"    return 1;\n"..
	"}",
	"data Enum1 =\n"..
	"    Value11 |\n"..
	"    Value12 |\n"..
	"    Value13 |\n"..
	"    Enum1s deriving (Eq)\n"..
	"--",
	"data Struct1A1X8 = Struct1A1X8 -- all\n"..
	"    [Struct1] -- next\n"..
	"    [Float] -- field1\n"..
	"    [Double] -- field2\n"..
	"    [[Int]] -- field3\n"..
	"    String -- field4\n"..
	"    [Int] -- field5\n"..
	"    Enum1 -- field6\n"..
	"    Enum2 -- field7\n"..
	"    deriving (Eq)\n"..
	"data Struct1A15X18 = Struct1A15X18 -- all\n"..
	"    Int -- field14\n"..
	"    [Int] -- field15\n"..
	"    [Struct2] -- field16\n"..
	"    [Struct2] -- field17\n"..
	"    deriving (Eq)\n"..
	"data Struct1A9X11 =\n"..
	"    Struct1A9X11B9X10 -- field6:Value11;field7:Value21,Value22,Value23\n"..
	"        Int -- field8\n"..
	"        Int | -- field9\n"..
	"    Struct1A9X11B11 Int | -- field10 -- field6:Value12;field7:Value21,Value22,Value23\n"..
	"    Struct1A9X11Bs deriving (Eq)\n"..
	"data Struct1A12X14 =\n"..
	"    Struct1A12X14B12 Int | -- field11 -- field6:Value12;field7:Value21\n"..
	"    Struct1A12X14B13 Int | -- field12 -- field6:Value12;field7:Value22,Value23\n"..
	"    Struct1A12X14B14 Int | -- field13 -- field6:Value13;field7:Value21,Value22,Value23\n"..
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
	"getStruct1Cfield3 :: Struct1 -> [[Int]]\n"..
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
	"getStruct1Cfield8 (Struct1 a1x8 (Struct1A9X11B9X10 a a10) a12x14 a15x18) = a\n"..
	"getStruct1Cfield9 :: Struct1 -> Int\n"..
	"getStruct1Cfield9 (Struct1 a1x8 (Struct1A9X11B9X10 a9 a) a12x14 a15x18) = a\n"..
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
	(
	"setStruct1Cnext :: Struct1 -> [Struct1] -> Struct1\n"..
	"setStruct1Cnext (Struct1 (Struct1A1X8 _ a2 a3 a4 a5 a6 a7 a8) a9x11 a12x14 a15x18) a = (Struct1 (Struct1A1X8 a a2 a3 a4 a5 a6 a7 a8) a9x11 a12x14 a15x18)\n"..
	"setStruct1Cfield1 :: Struct1 -> [Float] -> Struct1\n"..
	"setStruct1Cfield1 (Struct1 (Struct1A1X8 a1 _ a3 a4 a5 a6 a7 a8) a9x11 a12x14 a15x18) a = (Struct1 (Struct1A1X8 a1 a a3 a4 a5 a6 a7 a8) a9x11 a12x14 a15x18)\n"..
	"setStruct1Cfield2 :: Struct1 -> [Double] -> Struct1\n"..
	"setStruct1Cfield2 (Struct1 (Struct1A1X8 a1 a2 _ a4 a5 a6 a7 a8) a9x11 a12x14 a15x18) a = (Struct1 (Struct1A1X8 a1 a2 a a4 a5 a6 a7 a8) a9x11 a12x14 a15x18)\n"..
	"setStruct1Cfield3 :: Struct1 -> [[Int]] -> Struct1\n"..
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
	"setStruct1Cfield8 (Struct1 a1x8 (Struct1A9X11B9X10 _ a10) a12x14 a15x18) a = (Struct1 a1x8 (Struct1A9X11B9X10 a a10) a12x14 a15x18)\n"..
	"setStruct1Cfield9 :: Struct1 -> Int -> Struct1\n"..
	"setStruct1Cfield9 (Struct1 a1x8 (Struct1A9X11B9X10 a9 _) a12x14 a15x18) a = (Struct1 a1x8 (Struct1A9X11B9X10 a9 a) a12x14 a15x18)\n"..
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
	"assertHelp :: Int -> (a -> IO ()) -> [a] -> IO ()\n"..
	"assertHelp a _ []\n"..
	"    | a == 0 = return ()\n"..
	"    | otherwise = undefined\n"..
	"assertHelp a f (b:c)\n"..
	"    | a > 0 = (f b) >> (assertHelp (a-1) f c)\n"..
	"    | otherwise = undefined\n"..
	"condHelp :: Bool -> a -> IO a -> IO a\n"..
	"condHelp True _ a = a\n"..
	"condHelp False a _ = return a\n"..
	"firstHelp :: Eq a => a -> [a] -> IO a\n"..
	"firstHelp a [] = return a\n"..
	"firstHelp a (b:c)\n"..
	"    | a == b = firstHelp a c\n"..
	"    | otherwise = return b\n"..
	"--",
	"((a7 == Value12) && ((a8 == Value21) || (a8 == Value22)))",
	"((a8 == Value21) || (a8 == Value22))",
	"(a7 == Value12)",
	"True",
	"readStruct1 :: Int -> IO Struct1\n"..
	"readStruct1 idx = do\n"..
	"    a1 <- listHelp 0 (readStruct1 idx)\n"..
	"    a2 <- listHelp 2 (readOld idx)\n"..
	"    a3 <- listHelp 3 (readNum idx)\n"..
	"    a4 <- listHelp 2 (listHelp 2 (readInt idx))\n"..
	"    a5 <- fmap fst (readStr idx)\n"..
	"    a6 <- listHelp 3 (readInt idx)\n"..
	"    a7 <- readEnum1 idx\n"..
	"    a8 <- readEnum2 idx\n"..
	"    a1x8 <- return (Struct1A1X8 a1 a2 a3 a4 a5 a6 a7 a8)\n"..
	"    b9x10 <- condHelp (a7 == Value11) Struct1A9X11Bs (do\n"..
	"        a9 <- readInt idx\n"..
	"        a10 <- readInt idx\n"..
	"        return (Struct1A9X11B9X10 a9 a10))\n"..
	"    b11 <- condHelp (a7 == Value12) Struct1A9X11Bs (do\n"..
	"        a11 <- readInt idx\n"..
	"        return (Struct1A9X11B11 a11))\n"..
	"    a9x11 <- firstHelp Struct1A9X11Bs [b9x10,b11]\n"..
	"    b12 <- condHelp ((a7 == Value12) && (a8 == Value21)) Struct1A12X14Bs (do\n"..
	"        a12 <- readInt idx\n"..
	"        return (Struct1A12X14B12 a12))\n"..
	"    b13 <- condHelp ((a7 == Value12) && ((a8 == Value22) || (a8 == Value23))) Struct1A12X14Bs (do\n"..
	"        a13 <- readInt idx\n"..
	"        return (Struct1A12X14B13 a13))\n"..
	"    b14 <- condHelp (a7 == Value13) Struct1A12X14Bs (do\n"..
	"        a14 <- readInt idx\n"..
	"        return (Struct1A12X14B14 a14))\n"..
	"    a12x14 <- firstHelp Struct1A12X14Bs [b12,b13,b14]\n"..
	"    a15 <- readInt idx\n"..
	"    a16 <- listHelp a15 (readInt idx)\n"..
	"    a17 <- listHelp 2 (readStruct2 idx)\n"..
	"    a18 <- listHelp 2 (readStruct2 idx)\n"..
	"    a15x18 <- return (Struct1A15X18 a15 a16 a17 a18)\n"..
	"    return (Struct1 a1x8 a9x11 a12x14 a15x18)\n"..
	"--",
	"readEnum1F :: Int -> IO Enum1\n"..
	"readEnum1F 0 = return Value11\n"..
	"readEnum1F 1 = return Value12\n"..
	"readEnum1F 2 = return Value13\n"..
	"readEnum1F _ = return Enum1s\n"..
	"readEnum1 :: Int -> IO Enum1\n"..
	"readEnum1 idx = (readInt idx) >>= readEnum1F\n"..
	"writeEnum1F :: Enum1 -> Int\n"..
	"writeEnum1F Value11 = 0\n"..
	"writeEnum1F Value12 = 1\n"..
	"writeEnum1F Value13 = 2\n"..
	"writeEnum1F _ = 3\n"..
	"writeEnum1 :: Enum1 -> Int -> IO ()\n"..
	"writeEnum1 a idx = writeInt (writeEnum1F a) idx\n"..
	"--",
	"writeStruct1 :: Struct1 -> Int -> IO ()\n"..
	"writeStruct1 (Struct1 (Struct1A1X8 a1 a2 a3 a4 a5 a6 a7 a8) a9x11 a12x14 (Struct1A15X18 a15 a16 a17 a18)) idx = do\n"..
	"    assertHelp 0 (\\x -> writeStruct1 x idx) a1\n"..
	"    assertHelp 2 (\\x -> writeOld x idx) a2\n"..
	"    assertHelp 3 (\\x -> writeNum x idx) a3\n"..
	"    assertHelp 2 (assertHelp 2 (\\x -> writeInt x idx)) a4\n"..
	"    writeStr a5 True idx\n"..
	"    assertHelp 3 (\\x -> writeInt x idx) a6\n"..
	"    writeEnum1 a7 idx\n"..
	"    writeEnum2 a8 idx\n"..
	"    condHelp (a7 == Value11) () ((\\(Struct1A9X11B9X10 a9 a10) -> do\n"..
	"        writeInt a9 idx\n"..
	"        writeInt a10 idx) a9x11)\n"..
	"    condHelp (a7 == Value12) () ((\\(Struct1A9X11B11 a11) -> writeInt a11 idx) a9x11)\n"..
	"    condHelp ((a7 == Value12) && (a8 == Value21)) () ((\\(Struct1A12X14B12 a12) -> writeInt a12 idx) a12x14)\n"..
	"    condHelp ((a7 == Value12) && ((a8 == Value22) || (a8 == Value23))) () ((\\(Struct1A12X14B13 a13) -> writeInt a13 idx) a12x14)\n"..
	"    condHelp (a7 == Value13) () ((\\(Struct1A12X14B14 a14) -> writeInt a14 idx) a12x14)\n"..
	"    writeInt a15 idx\n"..
	"    assertHelp a15 (\\x -> writeInt x idx) a16\n"..
	"    assertHelp 2 (\\x -> writeStruct2 x idx) a17\n"..
	"    assertHelp 2 (\\x -> writeStruct2 x idx) a18\n"..
	"--",
	"function readEnum1(idx)\n"..
	"    val = readInt(idx)\n"..
	"    if (val == 0) then return \"Value11\"\n"..
	"    elseif (val == 1) then return \"Value12\"\n"..
	"    elseif (val == 2) then return \"Value13\"\n"..
	"    else return nil end\n"..
	"end\n"..
	"function writeEnum1(val,idx)\n"..
	"    if (val == \"Value11\") then writeInt(0,idx)\n"..
	"    elseif (val == \"Value12\") then writeInt(1,idx)\n"..
	"    elseif (val == \"Value13\") then writeInt(2,idx)\n"..
	"    else writeInt(4,idx) end\n"..
	"end\n"..
	"--",
	"function readStruct1(idx)\n"..
	"    local tab = {}\n"..
	"    tab[\"next\"] = {}\n"..
	"    local i1 = 1\n"..
	"    while (i1 <= 0) do\n"..
	"        tab[\"next\"][i1] = readStruct1(idx)\n"..
	"        i1 = i1 + 1\n"..
	"    end\n"..
	"    tab[\"field1\"] = {}\n"..
	"    local i1 = 1\n"..
	"    while (i1 <= 2) do\n"..
	"        tab[\"field1\"][i1] = readOld(idx)\n"..
	"        i1 = i1 + 1\n"..
	"    end\n"..
	"    tab[\"field2\"] = {}\n"..
	"    local i1 = 1\n"..
	"    while (i1 <= 3) do\n"..
	"        tab[\"field2\"][i1] = readNum(idx)\n"..
	"        i1 = i1 + 1\n"..
	"    end\n"..
	"    tab[\"field3\"] = {}\n"..
	"    local i1 = 1\n"..
	"    while (i1 <= 2) do\n"..
	"        tab[\"field3\"][i1] = {}\n"..
	"        local i2 = 1\n"..
	"        while (i2 <= 2) do\n"..
	"            tab[\"field3\"][i1][i2] = readInt(idx)\n"..
	"            i2 = i2 + 1\n"..
	"        end\n"..
	"        i1 = i1 + 1\n"..
	"    end\n"..
	"    tab[\"field4\"] = readStr(idx)\n"..
	"    tab[\"field5\"] = {}\n"..
	"    local i1 = 1\n"..
	"    while (i1 <= 3) do\n"..
	"        tab[\"field5\"][i1] = readInt(idx)\n"..
	"        i1 = i1 + 1\n"..
	"    end\n"..
	"    tab[\"field6\"] = readEnum1(idx)\n"..
	"    tab[\"field7\"] = readEnum2(idx)\n"..
	"    if (tab[\"field6\"] == \"Value11\") then\n"..
	"        tab[\"field8\"] = readInt(idx)\n"..
	"    end\n"..
	"    if (tab[\"field6\"] == \"Value11\") then\n"..
	"        tab[\"field9\"] = readInt(idx)\n"..
	"    end\n"..
	"    if (tab[\"field6\"] == \"Value12\") then\n"..
	"        tab[\"field10\"] = readInt(idx)\n"..
	"    end\n"..
	"    if ((tab[\"field6\"] == \"Value12\") and (tab[\"field7\"] == \"Value21\")) then\n"..
	"        tab[\"field11\"] = readInt(idx)\n"..
	"    end\n"..
	"    if ((tab[\"field6\"] == \"Value12\") and ((tab[\"field7\"] == \"Value22\") or (tab[\"field7\"] == \"Value23\"))) then\n"..
	"        tab[\"field12\"] = readInt(idx)\n"..
	"    end\n"..
	"    if (tab[\"field6\"] == \"Value13\") then\n"..
	"        tab[\"field13\"] = readInt(idx)\n"..
	"    end\n"..
	"    tab[\"field14\"] = readInt(idx)\n"..
	"    tab[\"field15\"] = {}\n"..
	"    local i1 = 1\n"..
	"    while (i1 <= tab[\"field14\"]) do\n"..
	"        tab[\"field15\"][i1] = readInt(idx)\n"..
	"        i1 = i1 + 1\n"..
	"    end\n"..
	"    tab[\"field16\"] = {}\n"..
	"    local i1 = 1\n"..
	"    while (i1 <= 2) do\n"..
	"        tab[\"field16\"][i1] = readStruct2(idx)\n"..
	"        i1 = i1 + 1\n"..
	"    end\n"..
	"    tab[\"field17\"] = {}\n"..
	"    local i1 = 1\n"..
	"    while (i1 <= 2) do\n"..
	"        tab[\"field17\"][i1] = readStruct2(idx)\n"..
	"        i1 = i1 + 1\n"..
	"    end\n"..
	"    return tab\n"..
	"end\n"..
	"--",
	"function writeStruct1(tab,idx)\n"..
	"    local i1 = 1\n"..
	"    while (i1 <= 0) do\n"..
	"        writeStruct1(tab[\"next\"][i1],idx)\n"..
	"        i1 = i1 + 1\n"..
	"    end\n"..
	"    local i1 = 1\n"..
	"    while (i1 <= 2) do\n"..
	"        writeOld(tab[\"field1\"][i1],idx)\n"..
	"        i1 = i1 + 1\n"..
	"    end\n"..
	"    local i1 = 1\n"..
	"    while (i1 <= 3) do\n"..
	"        writeNum(tab[\"field2\"][i1],idx)\n"..
	"        i1 = i1 + 1\n"..
	"    end\n"..
	"    local i1 = 1\n"..
	"    while (i1 <= 2) do\n"..
	"        local i2 = 1\n"..
	"        while (i2 <= 2) do\n"..
	"            writeInt(tab[\"field3\"][i1][i2],idx)\n"..
	"            i2 = i2 + 1\n"..
	"        end\n"..
	"        i1 = i1 + 1\n"..
	"    end\n"..
	"    writeStr(tab[\"field4\"],1,idx)\n"..
	"    local i1 = 1\n"..
	"    while (i1 <= 3) do\n"..
	"        writeInt(tab[\"field5\"][i1],idx)\n"..
	"        i1 = i1 + 1\n"..
	"    end\n"..
	"    writeEnum1(tab[\"field6\"],idx)\n"..
	"    writeEnum2(tab[\"field7\"],idx)\n"..
	"    if (tab[\"field6\"] == \"Value11\") then\n"..
	"        writeInt(tab[\"field8\"],idx)\n"..
	"    end\n"..
	"    if (tab[\"field6\"] == \"Value11\") then\n"..
	"        writeInt(tab[\"field9\"],idx)\n"..
	"    end\n"..
	"    if (tab[\"field6\"] == \"Value12\") then\n"..
	"        writeInt(tab[\"field10\"],idx)\n"..
	"    end\n"..
	"    if ((tab[\"field6\"] == \"Value12\") and (tab[\"field7\"] == \"Value21\")) then\n"..
	"        writeInt(tab[\"field11\"],idx)\n"..
	"    end\n"..
	"    if ((tab[\"field6\"] == \"Value12\") and ((tab[\"field7\"] == \"Value22\") or (tab[\"field7\"] == \"Value23\"))) then\n"..
	"        writeInt(tab[\"field12\"],idx)\n"..
	"    end\n"..
	"    if (tab[\"field6\"] == \"Value13\") then\n"..
	"        writeInt(tab[\"field13\"],idx)\n"..
	"    end\n"..
	"    writeInt(tab[\"field14\"],idx)\n"..
	"    local i1 = 1\n"..
	"    while (i1 <= tab[\"field14\"]) do\n"..
	"        writeInt(tab[\"field15\"][i1],idx)\n"..
	"        i1 = i1 + 1\n"..
	"    end\n"..
	"    local i1 = 1\n"..
	"    while (i1 <= 2) do\n"..
	"        writeStruct2(tab[\"field16\"][i1],idx)\n"..
	"        i1 = i1 + 1\n"..
	"    end\n"..
	"    local i1 = 1\n"..
	"    while (i1 <= 2) do\n"..
	"        writeStruct2(tab[\"field17\"][i1],idx)\n"..
	"        i1 = i1 + 1\n"..
	"    end\n"..
	"end\n"..
	"--"
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
-- MAIN
file = io.open("type.tmp","w")
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
file = io.open("type.tmp","r")
io.input(file)
for k,v in ipairs(Expected) do
	for key,val in ipairs(linesOf(Stimulus[k][1].." "..v)) do
		line = io.read();
		--print(line)
		if line ~= val then print("error1: "..line..":::"..val); assert(false) end
	end
end
line = io.read(); if line ~= nil then print("error2: "..line); assert(false) end
io.close(file)
