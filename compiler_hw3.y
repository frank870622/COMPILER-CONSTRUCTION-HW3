/*	Definition section */
%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
extern int yylineno;
extern int yylex();
extern char* yytext;   // Get current token from lex
extern char buf[256];  // Get current code line from lex

#define CLEANBUF { buf[0] = '\0'; }

void yyerror(char*);

/* Symbol table function - you can add new function if needed. */
int lookup_symbol(char*);
void create_symbol();
void insert_symbol(char*, int, int);
void dump_symbol(int);
void set_symbol_type();
void set_symbol_value(char*, float);
void set_function_parameter();
int get_symbol_type(char*);
float get_symbol_value(char*);
void clear_symbol(int);

typedef struct parse_table{
    //from 0~
    int index;
    char* name;
    // 1:variable 2:function 3:parameter
    int kind;
    // 1:int 2:float 3:bool 4:string 5:void
    int type;
    // from 0~
    int scope;
    // 1:int 2:float 3:bool 4:string 5:void
    int* attribute;
    float variable_value;
    int function_declartions;
    struct parse_table* next;
    struct parse_table* back;
}parse_table;

parse_table *head;

int scope_num = 0;
int index_num = 0;
int function_parameter_num = 0;
int variable_declare_count = 0;
int function_initial_flag = 0;
int function_has_declare_flag = 0;
int function_parameter_array[512];
char error_buf[128];
int had_print_flag = 0;
int dump_scope_flag = -1;
int syntax_error_flag = 0;
int print_error_flag = 0;

void reset_function_array();
void print_error(char*, char*);
void can_dump(int);


%}

/* Use variable or self-defined structure to represent
 * nonterminal and token type
 */
%union {
    int i_val;
    double f_val;
    char* string;
    char char_array[50];
    float variable_pack[2];   //0: value  1:
}

/* Token without return */
    /* Arithmetic */
%token ADD SUB MUL DIV MOD INC DEC
    /* Relational */
%token MT LT MTE LTE EQ NE
    /* Assignment */
%token ASGN ADDASGN SUBASGN MULASGN DIVASGN MODASGN
    /* Logical */
%token AND OR NOT
    /* Delimiters */    
%token LB RB LCB RCB LSB RSB COMMA
    /* Print Keywords */
%token PRINT 
    /* Condition and Loop Keywords */
%token IF ELSE FOR WHILE
    /* Declaration Keywords */
%token VOID INT FLOAT STRING BOOL
    /* boolean Keywords */
%token TRUE FALSE

%token RETURN
%token SEMICOLON
%token ENDFILE


/* Token with return, which need to sepcify type */
%token <i_val> I_CONST
%token <f_val> F_CONST
%token <string> STRING_TEXT
%token <string> ID

/* Nonterminal with return, which need to sepcify type */
%type <i_val> type
%type <char_array> function_declation_part1
%type <variable_pack> value
%type <variable_pack> value_stat
%type <variable_pack> lv3_arithmetic_stat
%type <variable_pack> lv2_arithmetic_stat
%type <variable_pack> arithmetic_stat
%type <variable_pack> initializer
%type <variable_pack> function_call


/* Yacc will start at this nonterminal */
%start program

/* Grammar section */
%%

program
    : program stats
    | stats
;

stats
    : stat
    | funtcion_declation
;

stat_list
    : stat_list stat
    | stat
;

stat
    : declaration_stat
    | compound_stat
    | expression_stat
    | print_func SEMICOLON
;

declaration_stat
    : type declaration SEMICOLON    {set_symbol_type($1);}
;

declaration
    : ID ASGN initializer   {
        if(lookup_symbol($1) != 0){
            insert_symbol($1, 1, -1);
            ++variable_declare_count;
            set_symbol_value($1, $3[0]);
        }
        else    print_error("Redeclared variable ", $1);
    }
    | ID    {
        if(lookup_symbol($1) != 0){
            insert_symbol($1, 1, -1);
            ++variable_declare_count;
            set_symbol_value($1, 0);
        }
        else    print_error("Redeclared variable ", $1);
    }
    | ID ASGN initializer COMMA declaration {
        if(lookup_symbol($1) != 0){
            insert_symbol($1, 1, -1);
            ++variable_declare_count;
            set_symbol_value($1, $3[0]);
        }
        else    print_error("Redeclared variable ", $1);
    }
    | ID COMMA declaration {
        if(lookup_symbol($1) != 0){
            insert_symbol($1, 1, -1);
            ++variable_declare_count;
            set_symbol_value($1, 0);
        }
        else    print_error("Redeclared variable ", $1);
    }
;

initializer
    : arithmetic_stat { $$[0] = $1[0]; $$[1] = $1[1]; }
;

arithmetic_stat
    : lv2_arithmetic_stat ADD arithmetic_stat { $$[0] = $1[0] + $3[0]; $$[1] = ($1[1]==2 || $3[1]==2)? 2:1; }
    | lv2_arithmetic_stat SUB arithmetic_stat { $$[0] = $1[0] - $3[0]; $$[1] = ($1[1]==2 || $3[1]==2)? 2:1; }
    | lv2_arithmetic_stat   { $$[0] = $1[0]; $$[1] = $1[1]; }
    | LB arithmetic_stat RB { $$[0] = $2[0]; $$[1] = $2[1]; }
;

lv2_arithmetic_stat
    : lv3_arithmetic_stat MUL lv2_arithmetic_stat { $$[0] = $1[0] * $3[0]; $$[1] = ($1[1]==2 || $3[1]==2)? 2:1; }
    | lv3_arithmetic_stat DIV lv2_arithmetic_stat 
    { 
        //printf("the value of diver %f\n", $3);
        if($3[0] == 0) {
            print_error("Divided by Zero", "");
            $$[0] = 0;
        }
        else{
            $$[0] = $1[0] / $3[0]; 
        }
        $$[1] = ($1[1]==2 || $3[1]==2)? 2:1;
    }
    | lv3_arithmetic_stat MOD lv2_arithmetic_stat
    { 
        if ($1[1] != 1 || $3[1] != 1)
            print_error("invalid operands to binary %", "");
        if($3[0] == 0) {
            print_error("Mod by Zero", "");
        }
        if($3[0] == 0 || $1[1] != 1 || $3[1] != 1){
            $$[0] = 0;
        }
        else{
            $$[0] = (int)$1[0] % (int)$3[0]; 
        }
        $$[1] = 1;
    }
    | lv3_arithmetic_stat   { $$[0] = $1[0]; $$[1] = $1[1]; }
    | LB lv2_arithmetic_stat RB { $$[0] = $2[0]; $$[1] = $2[1]; }
;

lv3_arithmetic_stat
    : INC lv3_arithmetic_stat   { $$[0] = $2[0] + 1; $$[1] = $2[1]; }
    | DEC lv3_arithmetic_stat   { $$[0] = $2[0] + 1; $$[1] = $2[1]; }
    | lv3_arithmetic_stat INC   { $$[0] = $1[0]; $$[1] = $1[1]; }
    | lv3_arithmetic_stat DEC   { $$[0] = $1[0]; $$[1] = $1[1]; }
    | value_stat                { $$[0] = $1[0]; $$[1] = $1[1]; }
    | LB lv3_arithmetic_stat RB { $$[0] = $2[0]; $$[1] = $2[1]; }
;



value_stat
    : ADD value_stat    { $$[0] = $2[0] * 1; $$[1] = $2[1]; }
    | SUB value_stat    { $$[0] = $2[0] * -1; $$[1] = $2[1]; }
    | STRING_TEXT   { }
    | value { $$[0] = $1[0]; $$[1] = $1[1]; }
    | LB value_stat RB  { $$[0] = $2[0]; $$[1] = $2[1]; }
;

value
    : I_CONST   { $$[0] = $1; $$[1] = 1; }
    | F_CONST   { $$[0] = $1; $$[1] = 2; }
    | TRUE      { $$[0] = 1; $$[1] = 1; }
    | FALSE     { $$[0] = 0; $$[1] = 1; }
    | ID {
        if(lookup_symbol($1) == -1)
            print_error("Undeclared variable ", $1);
        else{
            if(get_symbol_type($1) == 1){
                $$[0] = get_symbol_value($1);
                $$[1] = 1;
            }  
            else{
                $$[0] = get_symbol_value($1);
                $$[1] = 2;
            }  
        }
    }
    | function_call {$$[0] = $1[0]; $$[1] = $1[1]; }
;

funtcion_declation
    : function_declation_part1 function_declation_part2 {
        if(function_initial_flag == 1){
            if(function_parameter_num > 0)
                set_function_parameter();
            can_dump(scope_num);
        }
        else if(function_initial_flag == 0){
            if(function_parameter_num > 0)
                set_function_parameter();
            clear_symbol(scope_num);

            if(function_has_declare_flag == 1){
                print_error("Redeclared function ", $1);
            }
            function_has_declare_flag = 0;
        }
        --scope_num;
        function_initial_flag = 0;
    }
;

function_declation_part1
    : type ID LB {
        if(lookup_symbol($2) == -1){
            insert_symbol($2, 2, $1);
        }
        else {
            function_has_declare_flag = 1;
            
        }
        sprintf($$, "%s", $2);
        ++scope_num;
    }
;

function_declation_part2
    : function_parameter RB SEMICOLON   {function_initial_flag = 0;}
    | RB SEMICOLON  {function_initial_flag = 0;}
    | function_parameter RB LCB stat_list RCB   {function_initial_flag = 1;}
    | RB LCB stat_list RCB  {function_initial_flag = 1;}
;

function_parameter
    : type ID   {
        function_parameter_array[function_parameter_num] = $1;
        ++function_parameter_num;

        if(lookup_symbol($2) != 0){
            insert_symbol($2, 3, $1);
        }
        else    print_error("Redeclared variable ", $2);
    }
    | function_parameter COMMA type ID{
        function_parameter_array[function_parameter_num] = $3;
        ++function_parameter_num;

        if(lookup_symbol($4) != 0){
            insert_symbol($4, 3, $3);
        }
        else    print_error("Redeclared variable ", $4);
    }
;

compound_stat
    : if_else_stat
    | while_stat
;

if_else_stat
    : if_stat
    | if_stat else_stat
;

if_stat
    : if_stat_part1 if_stat_part2   {can_dump(scope_num); --scope_num;}
;

if_stat_part1
    : IF LB logical_stats RB    {++scope_num;}
;

if_stat_part2
    : SEMICOLON
    | stat
    | LCB stat_list RCB
;

else_stat
    : else_stat_part1 else_stat_part2   {can_dump(scope_num); --scope_num;}
;

else_stat_part1
    : ELSE  {++scope_num;}
;

else_stat_part2
    : stat
    | LCB stat_list RCB
    | SEMICOLON
;

logical_stats
    : logical_stats logical_operation logical_stat
    | logical_stat
;

logical_stat
    : value_stat MT value_stat
    | value_stat LT value_stat
    | value_stat MTE value_stat
    | value_stat LTE value_stat
    | value_stat EQ value_stat
    | value_stat NE value_stat
    | value_stat
;

logical_operation
    : AND
    | OR
;

while_stat
    : while_stat_part1 while_stat_part2 {can_dump(scope_num); --scope_num;}
;

while_stat_part1
    : WHILE LB logical_stats RB {++scope_num;}
;

while_stat_part2
    : SEMICOLON
    | stat
    | LCB stat_list RCB
;

expression_stat
    : assignment_stat SEMICOLON
    | function_call SEMICOLON
    | RETURN arithmetic_stat SEMICOLON
    | RETURN SEMICOLON
;

assignment_stat
    : ID ASGN arithmetic_stat {
        if(lookup_symbol($1) == -1)
            print_error("Undeclared variable ", $1);
    }
    | ID ADDASGN arithmetic_stat {
        if(lookup_symbol($1) == -1)
            print_error("Undeclared variable ", $1);
    }
    | ID SUBASGN arithmetic_stat {
        if(lookup_symbol($1) == -1)
            print_error("Undeclared variable ", $1);
    }
    | ID MULASGN arithmetic_stat {
        if(lookup_symbol($1) == -1)
            print_error("Undeclared variable ", $1);
    }
    | ID DIVASGN arithmetic_stat {
        if(lookup_symbol($1) == -1)
            print_error("Undeclared variable ", $1);
    }
    | ID MODASGN arithmetic_stat {
        if(lookup_symbol($1) == -1)
            print_error("Undeclared variable ", $1);
    }
    | arithmetic_stat
    | assignment_stat COMMA assignment_stat
;

function_call
    : ID LB function_send_parameter RB {
        if(lookup_symbol($1) == -1)
            print_error("Undeclared function ", $1);
        else{
            $$[0] = 100;
            $$[1] = get_symbol_type($1);
        }
    }
;

function_send_parameter
    : function_send_parameter COMMA arithmetic_stat
    | arithmetic_stat
    | logical_stats
;

print_func
    : PRINT LB value RB 
    | PRINT LB STRING_TEXT RB 
;


/* actions can be taken when meet the token or rule */
type
    : INT       {$$ = 1;}
    | FLOAT     {$$ = 2;}
    | BOOL      {$$ = 3;}
    | STRING    {$$ = 4;}
    | VOID      {$$ = 5;}
;

%%

/* C code section */
int main(int argc, char** argv)
{
    yylineno = 0;
    create_symbol();
    reset_function_array();

    yyparse();
    if(syntax_error_flag == 0){
        if(buf[0] != '\0'){
            printf("%d: %s\n", yylineno+1, buf);
            ++yylineno;
        }
            
        dump_symbol(scope_num);
        printf("\nTotal lines: %d \n",yylineno);
    }

    return 0;
}

void yyerror(char *s)
{
    if(strstr(s, "syntax") != NULL) syntax_error_flag = 1;

    if(print_error_flag != 0){
        if(had_print_flag == 0){
            if(buf[0] == '\n')
                printf("%d:%s", yylineno, buf);
            else
                printf("%d: %s\n", yylineno+1, buf);
            had_print_flag = 1;
        }
        print_error_flag = 0;
        printf("\n|-----------------------------------------------|\n");
        if(syntax_error_flag == 1)
            printf("| Error found in line %d: %s\n", yylineno+1, buf);
        else
            printf("| Error found in line %d: %s", yylineno, buf);
        printf("| %s", error_buf);
        printf("\n|-----------------------------------------------|\n\n");
    }

    if(had_print_flag == 0 && syntax_error_flag == 1){
        if(buf[0] == '\n')
            printf("%d:%s", yylineno, buf);
        else
            printf("%d: %s\n", yylineno+1, buf);
        had_print_flag = 1;
    }

    printf("\n|-----------------------------------------------|\n");
    if(syntax_error_flag == 1)
        printf("| Error found in line %d: %s\n", yylineno+1, buf);
    else 
        printf("| Error found in line %d: %s", yylineno, buf);
    printf("| %s", s);
    printf("\n|-----------------------------------------------|\n\n");

    //exit(-1);
}

void create_symbol() {
    head = (parse_table*)malloc(sizeof(parse_table));
    head->index = -1;
    head->name = NULL;
    head->kind = -1;
    head->type = -1;
    head->scope = -1;
    head->attribute = NULL;
    head->next = NULL;
    head->back = head;
}

void insert_symbol(char* Name, int Kind, int Type) {
    //printf("insert_symbol:%s, %d, %d\n", Name, Kind, Type);

    parse_table* temp = head;
    while(temp->next != NULL)   temp = temp->next;
    temp->next = (parse_table*)malloc(sizeof(parse_table));
    temp->next->back = temp;
    temp = temp->next;

    temp->index = index_num;
    temp->name = (char*)malloc(sizeof(char)*strlen(Name)+1);
    strncpy(temp->name, Name, strlen(Name));
    temp->kind = Kind;
    temp->type = Type;
    temp->scope = scope_num;

    /*
    if(function_parameter_num > 0){
        temp->attribute = (int*)malloc(sizeof(int)*function_parameter_num);
        for(int i = 0; i<function_parameter_num; ++i)
            temp->attribute[i] = function_parameter_array[i];
    }
    */

    temp->next = NULL;
    index_num++;
}

int lookup_symbol(char* Name) {
    //printf("lookup_symbol\n");
    //printf("Name: %s\n", Name);
    parse_table* temp = head;
    while(temp->next != NULL){
        temp = temp->next;
        if(strcmp(temp->name, Name) == 0 && temp->scope == scope_num)   return 0;
        if(strcmp(temp->name, Name) == 0 && temp->scope < scope_num)   return 1;
    }   
    return -1;
}

void dump_symbol(int dump_scope_num) {

    int table_has_element_flag = 0;
    parse_table* element = head;
    while(element->next != NULL){
        element = element->next;
        if(element->scope == dump_scope_num)
            table_has_element_flag = 1;
    }
    if(table_has_element_flag == 0) return;

    //printf("dump_symbol: %d\n", dump_scope_num);
    int index_count = 0;
    printf("\n%-10s%-10s%-12s%-10s%-10s%-10s\n\n",
           "Index", "Name", "Kind", "Type", "Scope", "Attribute");
    parse_table* temp = head;
    while(temp->next != NULL)
    {
        temp = temp->next;
        if(temp->scope == dump_scope_num){
            printf("%-10d", index_count);
            printf("%-10s", temp->name);

            if(temp->kind ==1)          printf("%-12s", "variable");
            else if (temp->kind ==2)    printf("%-12s", "function");
            else if (temp->kind ==3)    printf("%-12s", "parameter");

            if(temp->type == 1)         printf("%-10s", "int");
            else if(temp->type == 2)    printf("%-10s", "float");
            else if(temp->type == 3)    printf("%-10s", "bool");
            else if(temp->type == 4)    printf("%-10s", "string");
            else if(temp->type == 5)    printf("%-10s", "void");

            printf("%-10d", dump_scope_num);

            if(temp->attribute != NULL){
                if(temp->attribute[0] == 1)         printf("int");
                else if(temp->attribute[0] == 2)    printf("float");
                else if(temp->attribute[0] == 3)    printf("bool");
                else if(temp->attribute[0] == 4)    printf("string");
                else if(temp->attribute[0] == 5)    printf("void");
                for(int i=1; i< sizeof(temp->attribute)/sizeof(temp->attribute[0]); ++i){
                    if(temp->attribute[i] == 1)         printf(", int");
                    else if(temp->attribute[i] == 2)    printf(", float");
                    else if(temp->attribute[i] == 3)    printf(", bool");
                    else if(temp->attribute[i] == 4)    printf(", string");
                    else if(temp->attribute[i] == 5)    printf(", void");
                }
            }
            printf("\n");
            index_count++;
        }
    }
    printf("\n");
    clear_symbol(dump_scope_num);
}

void set_function_parameter(){
    parse_table *temp = head;
    while(temp -> next != NULL)     temp = temp -> next;
    while(temp->kind != 2)  temp = temp -> back;
    temp->attribute = (int*)malloc(sizeof(int)*function_parameter_num);
    for(int i=0; i<function_parameter_num; ++i){
        temp->attribute[i] = function_parameter_array[i];
    }
    reset_function_array();
}

void set_symbol_type(int Type){
    parse_table *temp = head;
    while(temp -> next != NULL)     temp = temp -> next;
    for(int i=0; i<variable_declare_count; ++i){
        temp->type = Type;
        temp = temp -> back;
    }

    variable_declare_count = 0;
}

void set_symbol_value(char* Name, float input_value){
    parse_table* temp = head;
    while(temp->next != NULL){
        temp = temp->next;
        if(strcmp(temp->name, Name) == 0 && temp->scope == scope_num){
            temp->variable_value = input_value;
            return;
        }

    }   
    temp = head;
    while(temp->next != NULL){
        temp = temp->next;
        if(strcmp(temp->name, Name) == 0 && temp->scope < scope_num){
            temp->variable_value = input_value; 
            return;
        }
    }   
}

int get_symbol_type(char* Name){
    parse_table* temp = head;
    while(temp->next != NULL){
        temp = temp->next;
        if(strcmp(temp->name, Name) == 0 && temp->scope == scope_num){
            return temp->type;
        }

    }   
    temp = head;
    while(temp->next != NULL){
        temp = temp->next;
        if(strcmp(temp->name, Name) == 0 && temp->scope < scope_num){
            return temp->type;
        }
    }   
}

float get_symbol_value(char* Name){
    parse_table* temp = head;
    while(temp->next != NULL){
        temp = temp->next;
        if(strcmp(temp->name, Name) == 0 && temp->scope == scope_num){
            return temp->variable_value;
        }

    }   
    temp = head;
    while(temp->next != NULL){
        temp = temp->next;
        if(strcmp(temp->name, Name) == 0 && temp->scope < scope_num){
             return temp->variable_value;
        }
    }   
}

void clear_symbol(int scope_num){
    if(scope_num != 0){
        parse_table* temp = head;

        while(temp->next != NULL){
            temp = temp -> next;
            if(temp->scope == scope_num){
                if(temp->next != NULL){
                    temp->back->next = temp->next;
                    temp->next->back = temp->back;
                }
                else temp->back->next = NULL;
                parse_table* deletenode = temp;
                temp = temp -> back;
                free(deletenode);
            }
        }

    }
}

void reset_function_array(){
    for(int i=0; i<512; ++i)
        function_parameter_array[i] = -1;
    function_parameter_num = 0;
}



void print_error(char* msg, char* Name){
    sprintf(error_buf, "%s%s", msg, Name);
    print_error_flag = 1;
}

void print_error_after_line(){
    if(print_error_flag != 0){
        print_error_flag = 0;
        yyerror(error_buf);
    }
    print_error_flag = 0;
}

void can_dump(int dump_scope_num){
    dump_scope_flag = dump_scope_num;
}