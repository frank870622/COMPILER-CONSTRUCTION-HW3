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
void set_function_parameter(char*);
int get_symbol_type(char*);
float get_symbol_value(char*);
int get_symbol_index(char*);

void clear_symbol(int);
int check_fun_error_initail(char*);
int check_fun_call_error(char*);
int find_stack_num_of_local_var(char*);
int check_global_variable(char*);

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
    int parameter_num;
    struct parse_table* next;
    struct parse_table* back;
}parse_table;

parse_table *head;

int scope_num = 0;
int index_num = 0;
int function_parameter_num = 0;
int function_call_parameter_num = 0;
int variable_declare_count = 0;
int function_initial_flag = 0;
int function_has_declare_flag = 0;
int function_parameter_array[512];
int function_call_parameter_array[512];
char error_buf[256];
int had_print_flag = 0;
int dump_scope_flag = -1;
int syntax_error_flag = 0;
int print_error_flag = 0;
char now_in_function_name[128];
char get_string_text_buf[128];
int logical_stat_mode_flag = -1; //0:MT 1:LT 2:MTE 3:LTE 4:EQ 5:NE
int jumb_label_num = 0;
int exit_label_num = 0;
int next_logical_num = 0;
int while_begin_num = 0;
int while_true_num = 0;
int while_false_num = 0;


parse_table* get_symbol_by_index(int);
parse_table* get_function_parse_by_name(char*);

int file_delete_flag = 0;

FILE *file; // To generate .j file for Jasmin

void reset_function_array();
void reset_function_call_array();
void print_error(char*, char*);
void can_dump(int);

/* code generation functions, just an example! */
void gencode_function();
void gencode_positive(float*);
void gencode_negative(float*);
void gencode_string_text(char*);
void gencode_INC_DEC(float*, int); //int == 0->INC, int == 1->DEC
void gencode_ADD_SUB_MUL_DIV_MOD(float*, float*, int);   //int == 0->ADD, == 1->SUB, == 2->MUL, == 3->DIV, == 4->MOD
void gencode_variable_load(float*);
void gencode_ASGN(float*);
void gencode_MT_LT_MTE_LTE_EQ_NE(float*, float*, int);  //int == 0->MT, == 1->LT, == 2->MTE, == 3->LTE, == 4->EQ, == 5->NE
void gencode_function_define(char*);
void gencode_function_call(char*);
void gencode_store_global(char*);
void gencdode_print_function(float*);

%}

/* Use variable or self-defined structure to represent
 * nonterminal and token type
 */
%union {
    int i_val;
    double f_val;
    char* string;
    char char_array[50];
    float variable_pack[6];   //0: value  1:type 2:has been load 3: load from id? 4: if(3) stack num(if global->index) 5:global
    int if_pack[2]; //0:EXIT_ num 1:NEXT_ num
    int while_pack[2];  //0:false_num 1:begin_num
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
%type <if_pack> if_stat_part1
%type <if_pack> if_stat
%type <while_pack> while_stat_part1_WHILE
%type <while_pack> while_stat_part1
%type <char_array> function_declation_part1
%type <variable_pack> value
%type <variable_pack> value_stat
%type <variable_pack> lv3_arithmetic_stat
%type <variable_pack> lv2_arithmetic_stat
%type <variable_pack> arithmetic_stat
%type <variable_pack> initializer
%type <variable_pack> function_call
%type <variable_pack> logical_stats
%type <variable_pack> logical_stat

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
    : declaration
    | compound_stat
    | expression_stat
    | print_func SEMICOLON
;

declaration
    : type ID ASGN initializer SEMICOLON {
        if(lookup_symbol($2) != 0){
            insert_symbol($2, 1, -1);
            ++variable_declare_count;
            set_symbol_value($2, $4[0]);
            set_symbol_type($1);

            if(scope_num == 0){
                gencode_function(".field public static ");
                gencode_function($2);
                gencode_function(" ");
                char tempbuf[32];
                if ($1 == 1){
                    gencode_function("I = ");
                    sprintf(tempbuf, "%d\n", (int)$4[0]);
                    gencode_function(tempbuf);
                }
                else if ($1 == 2){
                    gencode_function("F = ");
                    sprintf(tempbuf, "%f\n", (float)$4[0]);
                    gencode_function(tempbuf);
                }
                else if ($1 == 3){
                    gencode_function("Z = ");
                    sprintf(tempbuf, "%d\n", (int)$4[0]);
                    gencode_function(tempbuf);
                }
                else if ($1 == 4){
                    gencode_function("Ljava/lang/String; = ");
                    gencode_function(get_string_text_buf);
                    gencode_function("\n");
                }
                else if ($1 == 5){
                    gencode_function("V = ");
                    sprintf(tempbuf, "%f\n", (float)$4[0]);
                    gencode_function(tempbuf);
                }
            }
            else if(scope_num > 0){
                char tempbuf[32];
                gencode_variable_load($4);
                if($1 == 1 && $4[1] == 2)
                    gencode_function("f2i\n");
                else if($1 == 2 && $4[1] == 1)
                    gencode_function("i2f\n");

                if ($1 == 1){
                    gencode_function("istore ");
                    sprintf(tempbuf, "%d\n", find_stack_num_of_local_var($2));
                    gencode_function(tempbuf);
                }
                else if ($1 == 2){
                    gencode_function("fstore ");
                    sprintf(tempbuf, "%d\n", find_stack_num_of_local_var($2));
                    gencode_function(tempbuf);
                }
                else if ($1 == 3){
                    gencode_function("istore ");
                    sprintf(tempbuf, "%d\n", find_stack_num_of_local_var($2));
                    gencode_function(tempbuf);
                }
                else if ($1 == 4){
                    gencode_function("astore ");
                    sprintf(tempbuf, "%d\n", find_stack_num_of_local_var($2));
                    gencode_function(tempbuf);
                }
                else if ($1 == 5){
                    gencode_function("istore ");
                    sprintf(tempbuf, "%d\n", find_stack_num_of_local_var($2));
                    gencode_function(tempbuf);
                }
            }
        }
        else    print_error("Redeclared variable ", $2);
    }
    | type ID SEMICOLON  {
        if(lookup_symbol($2) != 0){
            insert_symbol($2, 1, -1);
            ++variable_declare_count;
            set_symbol_value($2, 0);
            set_symbol_type($1);

            if(scope_num == 0){
                gencode_function(".field public static ");
                gencode_function($2);
                gencode_function(" ");
                char tempbuf[32];
                if ($1 == 1){
                    gencode_function("I\n");
                }
                else if ($1 == 2){
                    gencode_function("F\n");
                }
                else if ($1 == 3){
                    gencode_function("Z\n");
                }
                else if ($1 == 4){
                    gencode_function("Ljava/lang/String;\n");
                }
                else if ($1 == 5){
                    gencode_function("V\n");
                }
            }
            else if(scope_num > 0){
                char tempbuf[32];
                if ($1 == 1){
                    gencode_function("ldc 0\n");
                    gencode_function("istore ");
                    sprintf(tempbuf, "%d\n", find_stack_num_of_local_var($2));
                    gencode_function(tempbuf);
                }
                else if ($1 == 2){
                    gencode_function("ldc 0\n");
                    gencode_function("i2f\n");
                    gencode_function("fstore ");
                    sprintf(tempbuf, "%d\n", find_stack_num_of_local_var($2));
                    gencode_function(tempbuf);
                }
                else if ($1 == 3){
                    gencode_function("ldc 0\n");
                    gencode_function("istore ");
                    sprintf(tempbuf, "%d\n", find_stack_num_of_local_var($2));
                    gencode_function(tempbuf);
                }
                else if ($1 == 4){
                    gencode_function("ldc ""\n");
                    gencode_function("astore ");
                    sprintf(tempbuf, "%d\n", find_stack_num_of_local_var($2));
                    gencode_function(tempbuf);
                }
                else if ($1 == 5){
                    gencode_function("ldc 0\n");
                    gencode_function("istore ");
                    sprintf(tempbuf, "%d\n", find_stack_num_of_local_var($2));
                    gencode_function(tempbuf);
                }
            }
        }
        else    print_error("Redeclared variable ", $2);
    }
;

initializer
    : arithmetic_stat { 
        $$[0] = $1[0];  $$[1] = $1[1]; 
        $$[2] = $1[2];  $$[3] = $1[3];
        $$[4] = $1[4];  $$[5] = $1[5];
    }
;

arithmetic_stat
    : lv2_arithmetic_stat ADD arithmetic_stat { 
        $$[0] = $1[0] + $3[0]; $$[1] = ($1[1]==2 || $3[1]==2)? 2:1; 
        $$[2] = 1;  $$[3] = -1;
        $$[4] = -1; $$[5] = -1;

        gencode_ADD_SUB_MUL_DIV_MOD($1, $3, 0);
    }
    | lv2_arithmetic_stat SUB arithmetic_stat { 
        $$[0] = $1[0] - $3[0]; $$[1] = ($1[1]==2 || $3[1]==2)? 2:1; 
        $$[2] = 1;  $$[3] = -1;
        $$[4] = -1; $$[5] = -1;

        gencode_ADD_SUB_MUL_DIV_MOD($1, $3, 1);
    }
    | lv2_arithmetic_stat   { 
        $$[0] = $1[0];  $$[1] = $1[1]; 
        $$[2] = $1[2];  $$[3] = $1[3];
        $$[4] = $1[4];  $$[5] = $1[5];
    }
    | LB arithmetic_stat RB { 
        $$[0] = $2[0];  $$[1] = $2[1];
        $$[2] = $2[2];  $$[3] = $2[3];
        $$[4] = $2[4];  $$[5] = $2[5];
    }
;

lv2_arithmetic_stat
    : lv3_arithmetic_stat MUL lv2_arithmetic_stat { 
        $$[0] = $1[0] * $3[0]; $$[1] = ($1[1]==2 || $3[1]==2)? 2:1; 
        $$[2] = 1;  $$[3] = -1;
        $$[4] = -1; $$[5] = -1;

        gencode_ADD_SUB_MUL_DIV_MOD($1, $3, 2);
    }
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
        $$[2] = 1;  $$[3] = -1;
        $$[4] = -1; $$[5] = -1;

        gencode_ADD_SUB_MUL_DIV_MOD($1, $3, 3);
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
        $$[2] = 1;  $$[3] = -1;
        $$[4] = -1; $$[5] = -1;

        gencode_ADD_SUB_MUL_DIV_MOD($1, $3, 4);
    }
    | lv3_arithmetic_stat   { 
        $$[0] = $1[0];  $$[1] = $1[1]; 
        $$[2] = $1[2];  $$[3] = $1[3];
        $$[4] = $1[4];  $$[5] = $1[5];
    }
    | LB lv2_arithmetic_stat RB { 
        $$[0] = $2[0];  $$[1] = $2[1]; 
        $$[2] = $2[2];  $$[3] = $2[3];
        $$[4] = $2[4];  $$[5] = $2[5];
    }
;

lv3_arithmetic_stat
    : INC lv3_arithmetic_stat   { 
        $$[0] = $2[0] + 1;  $$[1] = $2[1];
        $$[2] = 1;          $$[3] = 0;
        $$[4] = $2[4];      $$[5] = $2[5];
        gencode_INC_DEC($2, 0);

        char tempbuf[32];
        if($2[2] == 0){
            if($2[5] == 1){
                parse_table *temp_parse = get_symbol_by_index($2[4]);
                gencode_store_global(temp_parse->name);
                
                float var1[6];
                var1[0] = $2[0] + 1;   var1[1] = $2[1];
                var1[2] = 0;        var1[3] = 1;
                var1[4] = $2[4];    var1[5] = $2[5];

                gencode_variable_load(var1);
            }
            else if ($2[3] == 1){
                if ($2[1] == 1){
                    gencode_function("istore ");
                    sprintf(tempbuf, "%d\n", (int)$2[4]);
                    gencode_function(tempbuf);

                    gencode_function("iload ");
                    sprintf(tempbuf, "%d\n", (int)$2[4]);
                    gencode_function(tempbuf);
                }
                else if ($2[1] == 2){
                    gencode_function("fstore ");
                    sprintf(tempbuf, "%d\n", (int)$2[4]);
                    gencode_function(tempbuf);

                    gencode_function("fload ");
                    sprintf(tempbuf, "%d\n", (int)$2[4]);
                    gencode_function(tempbuf);
                }
                else if ($2[1] == 3){
                    gencode_function("istore ");
                    sprintf(tempbuf, "%d\n", (int)$2[4]);
                    gencode_function(tempbuf);

                    gencode_function("iload ");
                    sprintf(tempbuf, "%d\n", (int)$2[4]);
                    gencode_function(tempbuf);
                }
                else if ($2[1] == 4){
                    gencode_function("astore ");
                    sprintf(tempbuf, "%d\n", (int)$2[4]);
                    gencode_function(tempbuf);

                    gencode_function("aload ");
                    sprintf(tempbuf, "%d\n", (int)$2[4]);
                    gencode_function(tempbuf);
                }
                else if ($2[1] == 5){
                    gencode_function("istore ");
                    sprintf(tempbuf, "%d\n", (int)$2[4]);
                    gencode_function(tempbuf);

                    gencode_function("iload ");
                    sprintf(tempbuf, "%d\n", (int)$2[4]);
                    gencode_function(tempbuf);
                }
            }
        }
    }
    | DEC lv3_arithmetic_stat   { 
        $$[0] = $2[0] + 1;  $$[1] = $2[1]; 
        $$[2] = 1;          $$[3] = 0;
        $$[4] = $2[4];      $$[5] = $2[5];
        gencode_INC_DEC($2, 1);

        char tempbuf[32];
        if($2[2] == 0){
            if($2[5] == 1){
                parse_table *temp_parse = get_symbol_by_index($2[4]);
                gencode_store_global(temp_parse->name);
                
                float var1[6];
                var1[0] = $2[0] - 1;   var1[1] = $2[1];
                var1[2] = 0;        var1[3] = 1;
                var1[4] = $2[4];    var1[5] = $2[5];

                gencode_variable_load(var1);
            }
            else if ($2[3] == 1){
                if ($2[1] == 1){
                    gencode_function("istore ");
                    sprintf(tempbuf, "%d\n", (int)$2[4]);
                    gencode_function(tempbuf);

                    gencode_function("iload ");
                    sprintf(tempbuf, "%d\n", (int)$2[4]);
                    gencode_function(tempbuf);
                }
                else if ($2[1] == 2){
                    gencode_function("fstore ");
                    sprintf(tempbuf, "%d\n", (int)$2[4]);
                    gencode_function(tempbuf);

                    gencode_function("fload ");
                    sprintf(tempbuf, "%d\n", (int)$2[4]);
                    gencode_function(tempbuf);
                }
                else if ($2[1] == 3){
                    gencode_function("istore ");
                    sprintf(tempbuf, "%d\n", (int)$2[4]);
                    gencode_function(tempbuf);

                    gencode_function("iload ");
                    sprintf(tempbuf, "%d\n", (int)$2[4]);
                    gencode_function(tempbuf);
                }
                else if ($2[1] == 4){
                    gencode_function("astore ");
                    sprintf(tempbuf, "%d\n", (int)$2[4]);
                    gencode_function(tempbuf);

                    gencode_function("aload ");
                    sprintf(tempbuf, "%d\n", (int)$2[4]);
                    gencode_function(tempbuf);
                }
                else if ($2[1] == 5){
                    gencode_function("istore ");
                    sprintf(tempbuf, "%d\n", (int)$2[4]);
                    gencode_function(tempbuf);

                    gencode_function("iload ");
                    sprintf(tempbuf, "%d\n", (int)$2[4]);
                    gencode_function(tempbuf);
                }
            }
        }
    }
    | lv3_arithmetic_stat INC   { 
        $$[0] = $1[0];      $$[1] = $1[1]; 
        $$[2] = 1;          $$[3] = 0;
        $$[4] = $1[4];      $$[5] = $1[5];

        gencode_variable_load($1);

        gencode_INC_DEC($1, 0);

        char tempbuf[32];
        if($1[2] == 0){
            if($1[5] == 1){
                parse_table *temp_parse = get_symbol_by_index($1[4]);
                gencode_store_global(temp_parse->name);
            }
            else if ($1[3] == 1){
                if ($1[1] == 1){
                    gencode_function("istore ");
                    sprintf(tempbuf, "%d\n", (int)$1[4]);
                    gencode_function(tempbuf);
                }
                else if ($1[1] == 2){
                    gencode_function("fstore ");
                    sprintf(tempbuf, "%d\n", (int)$1[4]);
                    gencode_function(tempbuf);
                }
                else if ($1[1] == 3){
                    gencode_function("istore ");
                    sprintf(tempbuf, "%d\n", (int)$1[4]);
                    gencode_function(tempbuf);
                }
                else if ($1[1] == 4){
                    gencode_function("astore ");
                    sprintf(tempbuf, "%d\n", (int)$1[4]);
                    gencode_function(tempbuf);
                }
                else if ($1[1] == 5){
                    gencode_function("istore ");
                    sprintf(tempbuf, "%d\n", (int)$1[4]);
                    gencode_function(tempbuf);
                }
            }
        }
    }
    | lv3_arithmetic_stat DEC   { 
        $$[0] = $1[0]; $$[1] = $1[1];
        $$[2] = 1;          $$[3] = 0;
        $$[4] = $1[4];      $$[5] = $1[5];

        gencode_variable_load($1);

        gencode_INC_DEC($1, 1);

        char tempbuf[32];
        if($1[2] == 0){
            if($1[5] == 1){
                parse_table *temp_parse = get_symbol_by_index($1[4]);
                gencode_store_global(temp_parse->name);
            }
            else if ($1[3] == 1){
                if ($1[1] == 1){
                    gencode_function("istore ");
                    sprintf(tempbuf, "%d\n", (int)$1[4]);
                    gencode_function(tempbuf);
                }
                else if ($1[1] == 2){
                    gencode_function("fstore ");
                    sprintf(tempbuf, "%d\n", (int)$1[4]);
                    gencode_function(tempbuf);
                }
                else if ($1[1] == 3){
                    gencode_function("istore ");
                    sprintf(tempbuf, "%d\n", (int)$1[4]);
                    gencode_function(tempbuf);
                }
                else if ($1[1] == 4){
                    gencode_function("astore ");
                    sprintf(tempbuf, "%d\n", (int)$1[4]);
                    gencode_function(tempbuf);
                }
                else if ($1[1] == 5){
                    gencode_function("istore ");
                    sprintf(tempbuf, "%d\n", (int)$1[4]);
                    gencode_function(tempbuf);
                }
            }
        }
    }
    | value_stat    { 
        $$[0] = $1[0];  $$[1] = $1[1];
        $$[2] = $1[2];  $$[3] = $1[3];
        $$[4] = $1[4];  $$[5] = $1[5];
    }
    | LB lv3_arithmetic_stat RB { 
        $$[0] = $2[0];  $$[1] = $2[1]; 
        $$[2] = $2[2];  $$[3] = $2[3];
        $$[4] = $2[4];  $$[5] = $2[5];
    }
;



value_stat
    : ADD value_stat    { 
        $$[0] = $2[0] * 1;  $$[1] = $2[1];
        $$[2] = 1;          $$[3] = 0;
        $$[4] = $2[4];      $$[5] = $2[5];
        gencode_positive($2);
    }
    | SUB value_stat    { 
        $$[0] = $2[0] * -1; $$[1] = $2[1]; 
        $$[2] = 1;          $$[3] = 0;
        $$[4] = $2[4];      $$[5] = $2[5];
        gencode_negative($2);
    }
    | STRING_TEXT   { 
        $$[0] = -1; $$[1] = 4; 
        $$[2] = 1;  $$[3] = 0;
        $$[4] = -1; $$[5] = 0;
        gencode_string_text($1);
        sprintf(get_string_text_buf, "%s", $1);
    }
    | value { 
        $$[0] = $1[0];  $$[1] = $1[1];
        $$[2] = $1[2];  $$[3] = $1[3];
        $$[4] = $1[4];  $$[5] = $1[5];
    }
    | LB value_stat RB  { 
        $$[0] = $2[0];  $$[1] = $2[1];
        $$[2] = $2[2];  $$[3] = $2[3];
        $$[4] = $2[4];  $$[5] = $2[5];
    }
;

value
    : I_CONST   {
        $$[0] = $1; $$[1] = 1;
        $$[2] = 0;  $$[3] = 0;
        $$[4] = -1; $$[5] = 0;
    }
    | F_CONST   { 
        $$[0] = $1; $$[1] = 2;
        $$[2] = 0;  $$[3] = 0;
        $$[4] = -1; $$[5] = 0;
    }
    | TRUE      { 
        $$[0] = 1;  $$[1] = 3;
        $$[2] = 0;  $$[3] = 0;
        $$[4] = -1; $$[5] = 0;
    }
    | FALSE     { 
        $$[0] = 0;  $$[1] = 3; 
        $$[2] = 0;  $$[3] = 0;
        $$[4] = -1; $$[5] = 0;
    }
    | ID {
        if(lookup_symbol($1) == -1)
            print_error("Undeclared variable ", $1);
        else{
            if(get_symbol_type($1) == 4){
                $$[0] = -1;
                $$[1] = 4;
                $$[2] = 1;  $$[3] = 1;
                $$[4] = -1;
                char tempbuf[32];

                if(check_global_variable($1)){
                    gencode_function("getstatic compiler_hw3/");
                    gencode_function($1);
                    gencode_function(" Ljava/lang/String;\n");
                }
                else{
                    gencode_function("aload ");
                    sprintf(tempbuf, "%d\n", find_stack_num_of_local_var($1));
                    gencode_function(tempbuf);
                }
            }
            else{
                $$[0] = get_symbol_value($1);
                $$[1] = get_symbol_type($1);
                $$[2] = 0;  $$[3] = 1;
                $$[4] = find_stack_num_of_local_var($1);
            }
            $$[5] = check_global_variable($1);
            if(check_global_variable($1))   $$[4] = get_symbol_index($1);
        }
    }
    | function_call {$$[0] = $1[0]; $$[1] = $1[1]; }
;

funtcion_declation
    : function_declation_part1 function_declation_part2 {
        if(function_initial_flag == 1){
            if(function_has_declare_flag == 1){
                if(check_fun_error_initail($1) == -1)
                    print_error("function formal parameter is not the same ", $1);
            }
            if(function_parameter_num > 0)
                set_function_parameter($1);
            can_dump(scope_num);
        }
        else if(function_initial_flag == 0){
            if(function_parameter_num > 0)
                set_function_parameter($1);
            clear_symbol(scope_num);

            if(function_has_declare_flag == 1){
                print_error("Redeclared function ", $1);
            }
        }
        function_has_declare_flag = 0;
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
        sprintf(now_in_function_name, "%s", $2);
        ++scope_num;
    }
;

function_declation_part2
    : function_parameter RB SEMICOLON   {
        function_initial_flag = 0;
    }
    | RB SEMICOLON  {
        function_initial_flag = 0;
    }
    | function_parameter function_declation_RB1 function_declation_part3   {
        function_initial_flag = 1;
    }
    | function_declation_RB2 function_declation_part3  
    {
        function_initial_flag = 1;
        function_parameter_num = 0;
    }
;

function_declation_RB1
    : RB {
        gencode_function_define(now_in_function_name);
    }
;

function_declation_RB2
    : RB {
        function_parameter_num = 0;
        gencode_function_define(now_in_function_name);
    }
;

function_declation_part3
    : LCB stat_list RCB {
        gencode_function(".end method\n");
    }
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
    : if_stat {
        char tempbuf[32];

        sprintf(tempbuf, "EXIT_%d", $1[1]);
        gencode_function(tempbuf);
        gencode_function(":\n");
    }
    | if_stat else_stat {
        char tempbuf[32];

        sprintf(tempbuf, "EXIT_%d", $1[1]);
        gencode_function(tempbuf);
        gencode_function(":\n");
    }
;

if_stat
    : if_stat_part1 if_stat_part2   {
        can_dump(scope_num); --scope_num;

        $$[0] = $1[0];

        char tempbuf[32];
        gencode_function("goto ");
        sprintf(tempbuf, "EXIT_%d\n", $1[0]);
        gencode_function(tempbuf);

        sprintf(tempbuf, "NEXT_%d", $1[1]);
        gencode_function(tempbuf);
        gencode_function(":\n");

    }
;

if_stat_part1
    : IF LB logical_stats RB    {
        ++scope_num;
        char tempbuf[32];
        char tempbuf2[32];
        if(logical_stat_mode_flag == 0)
            gencode_function("ifgt ");
        else if(logical_stat_mode_flag == 1)
            gencode_function("iflt ");
        else if(logical_stat_mode_flag == 2)
            gencode_function("ifge ");
        else if(logical_stat_mode_flag == 3)
            gencode_function("ifle ");
        else if(logical_stat_mode_flag == 4)
            gencode_function("ifeq ");
        else if(logical_stat_mode_flag == 5)
            gencode_function("ifne ");

        sprintf(tempbuf, "Label_%d", jumb_label_num);
        gencode_function(tempbuf);
        gencode_function("\n");

        gencode_function("goto ");
        sprintf(tempbuf2, "NEXT_%d\n", next_logical_num);
        gencode_function(tempbuf2);
        $$[1] = next_logical_num;
        ++next_logical_num;

        $$[0] = exit_label_num;
        ++exit_label_num;

        gencode_function(tempbuf);
        gencode_function(":\n");
        ++jumb_label_num;
    }
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
    : logical_stat  { 
        $$[0] = $1[0];  $$[1] = $1[1]; 
        $$[2] = $1[2];  $$[3] = $1[3];
        $$[4] = $1[4];  $$[5] = $1[5];
    }
;

logical_stat
    : arithmetic_stat MT arithmetic_stat
    {
        if($1[0] > $3[0])   $$[0] = 1;
        else                $$[0] = 0;
        $$[1] = 3;
        $$[2] = 1;  $$[3] = 0;
        $$[4] = -1; $$[5] = -1;

        gencode_MT_LT_MTE_LTE_EQ_NE($1, $3, 0);

    }
    | arithmetic_stat LT arithmetic_stat
    {
        if($1[0] < $3[0])   $$[0] = 1;
        else                $$[0] = 0;
        $$[1] = 3;
        $$[2] = 1;  $$[3] = 0;
        $$[4] = -1; $$[5] = -1;

        gencode_MT_LT_MTE_LTE_EQ_NE($1, $3, 1);
    }
    | arithmetic_stat MTE arithmetic_stat
    {
        if($1[0] >= $3[0])  $$[0] = 1;
        else                $$[0] = 0;
        $$[1] = 3;
        $$[2] = 1;  $$[3] = 0;
        $$[4] = -1; $$[5] = -1;

        gencode_MT_LT_MTE_LTE_EQ_NE($1, $3, 2);
    }
    | arithmetic_stat LTE arithmetic_stat
    {
        if($1[0] <= $3[0])  $$[0] = 1;
        else                $$[0] = 0;
        $$[1] = 3;
        $$[2] = 1;  $$[3] = 0;
        $$[4] = -1; $$[5] = -1;

        gencode_MT_LT_MTE_LTE_EQ_NE($1, $3, 3);
    }
    | arithmetic_stat EQ arithmetic_stat
    {
        if($1[0] == $3[0])  $$[0] = 1;
        else                $$[0] = 0;
        $$[1] = 3;
        $$[2] = 1;  $$[3] = 0;
        $$[4] = -1; $$[5] = -1;

        gencode_MT_LT_MTE_LTE_EQ_NE($1, $3, 4);
    }
    | arithmetic_stat NE arithmetic_stat
    {
        if($1[0] != $3[0])  $$[0] = 1;
        else                $$[0] = 0;
        $$[1] = 3;
        $$[2] = 1;  $$[3] = 0;
        $$[4] = -1; $$[5] = -1;

        gencode_MT_LT_MTE_LTE_EQ_NE($1, $3, 5);
    }
    | arithmetic_stat   { 
        $$[0] = $1[0];  $$[1] = $1[1];
        $$[2] = $1[2];  $$[3] = $1[3];
        $$[4] = $1[4];  $$[5] = $1[5];
    }
;

while_stat
    : while_stat_part1 while_stat_part2 {
        can_dump(scope_num); --scope_num;

        char tempbuf[32];

        gencode_function("goto ");
        sprintf(tempbuf, "WHILE_BEGIN_%d\n", $1[1]);
        gencode_function(tempbuf);

        sprintf(tempbuf, "WHILE_FALSE_%d", $1[0]);
        gencode_function(tempbuf);
        gencode_function(":\n");
    }
;

while_stat_part1
    : while_stat_part1_WHILE LB logical_stats RB {
        ++scope_num;

        if(logical_stat_mode_flag == 0)
            gencode_function("ifgt ");
        else if(logical_stat_mode_flag == 1)
            gencode_function("iflt ");
        else if(logical_stat_mode_flag == 2)
            gencode_function("ifge ");
        else if(logical_stat_mode_flag == 3)
            gencode_function("ifle ");
        else if(logical_stat_mode_flag == 4)
            gencode_function("ifeq ");
        else if(logical_stat_mode_flag == 5)
            gencode_function("ifne ");
        
        char tempbuf[32];
        char tempbuf2[32];

        sprintf(tempbuf, "WHILE_TRUE_%d", while_true_num);
        gencode_function(tempbuf);
        gencode_function("\n");

        gencode_function("goto ");
        sprintf(tempbuf2, "WHILE_FALSE_%d\n", while_false_num);
        gencode_function(tempbuf2);

        gencode_function(tempbuf);
        gencode_function(":\n");

        $$[1] = $1[1];
        $$[0] = while_false_num;

        ++while_true_num;
        ++while_false_num;
    }
;

while_stat_part1_WHILE
    : WHILE {
        char tempbuf[32];
        sprintf(tempbuf, "WHILE_BEGIN_%d", while_begin_num);
        gencode_function(tempbuf);
        gencode_function(":\n");

        $$[1] = while_begin_num;
        ++while_begin_num;
    }
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
    {
        if(get_symbol_type(now_in_function_name) != $2[1] && get_symbol_type(now_in_function_name) > 2){
            print_error("function return type is not the same ", now_in_function_name);
        }
        else{
            gencode_variable_load($2);
            if(get_symbol_type(now_in_function_name) == 1){
                if($2[1] == 2)  gencode_function("f2i\n");
                gencode_function("ireturn\n");
            }
            else if(get_symbol_type(now_in_function_name) == 2){
                if($2[1] == 1)  gencode_function("i2f\n");
                gencode_function("freturn\n");
            }
            else if(get_symbol_type(now_in_function_name) == 3){
                if($2[1] == 2)  gencode_function("f2i\n");
                gencode_function("ireturn\n");
            }
            else if(get_symbol_type(now_in_function_name) == 4){
                if($2[1] == 2)  gencode_function("f2i\n");
                gencode_function("ireturn\n");
            }
        }
    }
    | RETURN SEMICOLON
    {
        if(get_symbol_type(now_in_function_name) != 5){
            print_error("function return type is not the same ", now_in_function_name);
        }
        else{
            gencode_function("return\n");
        }
    }
;

assignment_stat
    : ID ASGN arithmetic_stat {
        if(lookup_symbol($1) == -1)
            print_error("Undeclared variable ", $1);
        else{
            char tempbuf[32];
            gencode_variable_load($3);
            if(check_global_variable($1) == 1){
                if(get_symbol_type($1) == 1 && $3[1] == 2) gencode_function("f2i\n");
                if(get_symbol_type($1) == 2 && $3[1] == 1) gencode_function("i2f\n");
                if(get_symbol_type($1) == 3 && $3[1] == 2) gencode_function("f2i\n");
                gencode_store_global($1);
            }
            else{
                if (get_symbol_type($1) == 1){
                        if($3[1] == 2)  gencode_function("f2i\n");
                        gencode_function("istore ");
                        sprintf(tempbuf, "%d\n", find_stack_num_of_local_var($1));
                        gencode_function(tempbuf);
                    }
                    else if (get_symbol_type($1) == 2){
                        if($3[1] == 1) gencode_function("i2f\n");
                        gencode_function("fstore ");
                        sprintf(tempbuf, "%d\n", find_stack_num_of_local_var($1));
                        gencode_function(tempbuf);
                    }
                    else if (get_symbol_type($1) == 3){
                        if($3[1] == 2)  gencode_function("f2i\n");
                        gencode_function("istore ");
                        sprintf(tempbuf, "%d\n", find_stack_num_of_local_var($1));
                        gencode_function(tempbuf);
                    }
                    else if (get_symbol_type($1) == 4){
                        gencode_function("astore ");
                        sprintf(tempbuf, "%d\n", find_stack_num_of_local_var($1));
                        gencode_function(tempbuf);
                    }
                    else if (get_symbol_type($1) == 5){
                        gencode_function("istore ");
                        sprintf(tempbuf, "%d\n", find_stack_num_of_local_var($1));
                        gencode_function(tempbuf);
                }
            }
        }
    }
    | ID ADDASGN arithmetic_stat {
        if(lookup_symbol($1) == -1)
            print_error("Undeclared variable ", $1);
        else{
            char tempbuf[32];
            float var1[6];
            var1[0] = get_symbol_value($1);
            var1[1] = get_symbol_type($1);
            var1[2] = 0;
            var1[3] = 1;
            var1[4] = find_stack_num_of_local_var($1);
            var1[5] = 0;

            gencode_ADD_SUB_MUL_DIV_MOD(var1, $3, 0);

            var1[0] = var1[0] + $3[0];
            var1[1] = (var1[1] == 2 || $3[1] == 2)? 2:1;
            var1[2] = 1;

            if(check_global_variable($1) == 1){
                if(get_symbol_type($1) == 1 && $3[1] == 2) gencode_function("f2i\n");
                if(get_symbol_type($1) == 2 && $3[1] == 1) gencode_function("i2f\n");
                if(get_symbol_type($1) == 3 && $3[1] == 2) gencode_function("f2i\n");
                gencode_store_global($1);
            }
            else{
                if (get_symbol_type($1) == 1){
                    if(var1[1] == 2)  gencode_function("f2i\n");
                    gencode_function("istore ");
                    sprintf(tempbuf, "%d\n", find_stack_num_of_local_var($1));
                    gencode_function(tempbuf);
                }
                else if (get_symbol_type($1) == 2){
                    if(var1[1] == 1) gencode_function("i2f\n");
                    gencode_function("fstore ");
                    sprintf(tempbuf, "%d\n", find_stack_num_of_local_var($1));
                    gencode_function(tempbuf);
                }
                else if (get_symbol_type($1) == 3){
                    if(var1[1] == 2)  gencode_function("f2i\n");
                    gencode_function("istore ");
                    sprintf(tempbuf, "%d\n", find_stack_num_of_local_var($1));
                    gencode_function(tempbuf);
                }
                else if (get_symbol_type($1) == 4){
                    gencode_function("astore ");
                    sprintf(tempbuf, "%d\n", find_stack_num_of_local_var($1));
                    gencode_function(tempbuf);
                }
                else if (get_symbol_type($1) == 5){
                    gencode_function("istore ");
                    sprintf(tempbuf, "%d\n", find_stack_num_of_local_var($1));
                    gencode_function(tempbuf);
                }
            }
        }
    }
    | ID SUBASGN arithmetic_stat {
        if(lookup_symbol($1) == -1)
            print_error("Undeclared variable ", $1);
        else{
            char tempbuf[32];
            float var1[6];
            var1[0] = get_symbol_value($1);
            var1[1] = get_symbol_type($1);
            var1[2] = 0;
            var1[3] = 1;
            var1[4] = find_stack_num_of_local_var($1);
            var1[5] = 0;

            gencode_ADD_SUB_MUL_DIV_MOD(var1, $3, 1);

            var1[0] = var1[0] - $3[0];
            var1[1] = (var1[1] == 2 || $3[1] == 2)? 2:1;
            var1[2] = 1;

            if(check_global_variable($1) == 1){
                if(get_symbol_type($1) == 1 && $3[1] == 2) gencode_function("f2i\n");
                if(get_symbol_type($1) == 2 && $3[1] == 1) gencode_function("i2f\n");
                if(get_symbol_type($1) == 3 && $3[1] == 2) gencode_function("f2i\n");
                gencode_store_global($1);
            }
            else{
                if (get_symbol_type($1) == 1){
                    if(var1[1] == 2)  gencode_function("f2i\n");
                    gencode_function("istore ");
                    sprintf(tempbuf, "%d\n", find_stack_num_of_local_var($1));
                    gencode_function(tempbuf);
                }
                else if (get_symbol_type($1) == 2){
                    if(var1[1] == 1) gencode_function("i2f\n");
                    gencode_function("fstore ");
                    sprintf(tempbuf, "%d\n", find_stack_num_of_local_var($1));
                    gencode_function(tempbuf);
                }
                else if (get_symbol_type($1) == 3){
                    if(var1[1] == 2)  gencode_function("f2i\n");
                    gencode_function("istore ");
                    sprintf(tempbuf, "%d\n", find_stack_num_of_local_var($1));
                    gencode_function(tempbuf);
                }
                else if (get_symbol_type($1) == 4){
                    gencode_function("astore ");
                    sprintf(tempbuf, "%d\n", find_stack_num_of_local_var($1));
                    gencode_function(tempbuf);
                }
                else if (get_symbol_type($1) == 5){
                    gencode_function("istore ");
                    sprintf(tempbuf, "%d\n", find_stack_num_of_local_var($1));
                    gencode_function(tempbuf);
                }
            }
        }
    }
    | ID MULASGN arithmetic_stat {
        if(lookup_symbol($1) == -1)
            print_error("Undeclared variable ", $1);
        else{
            char tempbuf[32];
            float var1[6];
            var1[0] = get_symbol_value($1);
            var1[1] = get_symbol_type($1);
            var1[2] = 0;
            var1[3] = 1;
            var1[4] = find_stack_num_of_local_var($1);
            var1[5] = 0;

            gencode_ADD_SUB_MUL_DIV_MOD(var1, $3, 2);

            var1[0] = var1[0] * $3[0];
            var1[1] = (var1[1] == 2 || $3[1] == 2)? 2:1;
            var1[2] = 1;

            if(check_global_variable($1) == 1){
                if(get_symbol_type($1) == 1 && $3[1] == 2) gencode_function("f2i\n");
                if(get_symbol_type($1) == 2 && $3[1] == 1) gencode_function("i2f\n");
                if(get_symbol_type($1) == 3 && $3[1] == 2) gencode_function("f2i\n");
                gencode_store_global($1);
            }
            else{
                if (get_symbol_type($1) == 1){
                    if(var1[1] == 2)  gencode_function("f2i\n");
                    gencode_function("istore ");
                    sprintf(tempbuf, "%d\n", find_stack_num_of_local_var($1));
                    gencode_function(tempbuf);
                }
                else if (get_symbol_type($1) == 2){
                    if(var1[1] == 1) gencode_function("i2f\n");
                    gencode_function("fstore ");
                    sprintf(tempbuf, "%d\n", find_stack_num_of_local_var($1));
                    gencode_function(tempbuf);
                }
                else if (get_symbol_type($1) == 3){
                    if(var1[1] == 2)  gencode_function("f2i\n");
                    gencode_function("istore ");
                    sprintf(tempbuf, "%d\n", find_stack_num_of_local_var($1));
                    gencode_function(tempbuf);
                }
                else if (get_symbol_type($1) == 4){
                    gencode_function("astore ");
                    sprintf(tempbuf, "%d\n", find_stack_num_of_local_var($1));
                    gencode_function(tempbuf);
                }
                else if (get_symbol_type($1) == 5){
                    gencode_function("istore ");
                    sprintf(tempbuf, "%d\n", find_stack_num_of_local_var($1));
                    gencode_function(tempbuf);
                }
            }
        }
    }
    | ID DIVASGN arithmetic_stat {
        if(lookup_symbol($1) == -1)
            print_error("Undeclared variable ", $1);
        else if($3[0] == 0){
            print_error("Divided by Zero", "");
        }
        else{
            char tempbuf[32];
            float var1[6];
            var1[0] = get_symbol_value($1);
            var1[1] = get_symbol_type($1);
            var1[2] = 0;
            var1[3] = 1;
            var1[4] = find_stack_num_of_local_var($1);
            var1[5] = 0;

            gencode_ADD_SUB_MUL_DIV_MOD(var1, $3, 3);

            var1[0] = var1[0] / $3[0];
            var1[1] = (var1[1] == 2 || $3[1] == 2)? 2:1;
            var1[2] = 1;

            if(check_global_variable($1) == 1){
                if(get_symbol_type($1) == 1 && $3[1] == 2) gencode_function("f2i\n");
                if(get_symbol_type($1) == 2 && $3[1] == 1) gencode_function("i2f\n");
                if(get_symbol_type($1) == 3 && $3[1] == 2) gencode_function("f2i\n");
                gencode_store_global($1);
            }
            else{
                if (get_symbol_type($1) == 1){
                    if(var1[1] == 2)  gencode_function("f2i\n");
                    gencode_function("istore ");
                    sprintf(tempbuf, "%d\n", find_stack_num_of_local_var($1));
                    gencode_function(tempbuf);
                }
                else if (get_symbol_type($1) == 2){
                    if(var1[1] == 1) gencode_function("i2f\n");
                    gencode_function("fstore ");
                    sprintf(tempbuf, "%d\n", find_stack_num_of_local_var($1));
                    gencode_function(tempbuf);
                }
                else if (get_symbol_type($1) == 3){
                    if(var1[1] == 2)  gencode_function("f2i\n");
                    gencode_function("istore ");
                    sprintf(tempbuf, "%d\n", find_stack_num_of_local_var($1));
                    gencode_function(tempbuf);
                }
                else if (get_symbol_type($1) == 4){
                    gencode_function("astore ");
                    sprintf(tempbuf, "%d\n", find_stack_num_of_local_var($1));
                    gencode_function(tempbuf);
                }
                else if (get_symbol_type($1) == 5){
                    gencode_function("istore ");
                    sprintf(tempbuf, "%d\n", find_stack_num_of_local_var($1));
                    gencode_function(tempbuf);
                }
            }
        }
    }
    | ID MODASGN arithmetic_stat {
        if(lookup_symbol($1) == -1)
            print_error("Undeclared variable ", $1);
        else if (get_symbol_type($1) != 1 || $3[1] != 1){
            print_error("invalid operands to binary %", "");
        }
        else if ($3[0] == 0){
            print_error("Mod by Zero", "");
        }
        else{
            char tempbuf[32];
            float var1[6];
            var1[0] = get_symbol_value($1);
            var1[1] = get_symbol_type($1);
            var1[2] = 0;
            var1[3] = 1;
            var1[4] = find_stack_num_of_local_var($1);
            var1[5] = 0;

            gencode_ADD_SUB_MUL_DIV_MOD(var1, $3, 4);

            var1[0] = (int)var1[0] % (int)$3[0];
            var1[1] = 1;
            var1[2] = 1;

            if(check_global_variable($1) == 1){
                if(get_symbol_type($1) == 1 && $3[1] == 2) gencode_function("f2i\n");
                if(get_symbol_type($1) == 2 && $3[1] == 1) gencode_function("i2f\n");
                if(get_symbol_type($1) == 3 && $3[1] == 2) gencode_function("f2i\n");
                gencode_store_global($1);
            }
            else{
                if (get_symbol_type($1) == 1){
                    if(var1[1] == 2)  gencode_function("f2i\n");
                    gencode_function("istore ");
                    sprintf(tempbuf, "%d\n", find_stack_num_of_local_var($1));
                    gencode_function(tempbuf);
                }
                else if (get_symbol_type($1) == 2){
                    if(var1[1] == 1) gencode_function("i2f\n");
                    gencode_function("fstore ");
                    sprintf(tempbuf, "%d\n", find_stack_num_of_local_var($1));
                    gencode_function(tempbuf);
                }
                else if (get_symbol_type($1) == 3){
                    if(var1[1] == 2)  gencode_function("f2i\n");
                    gencode_function("istore ");
                    sprintf(tempbuf, "%d\n", find_stack_num_of_local_var($1));
                    gencode_function(tempbuf);
                }
                else if (get_symbol_type($1) == 4){
                    gencode_function("astore ");
                    sprintf(tempbuf, "%d\n", find_stack_num_of_local_var($1));
                    gencode_function(tempbuf);
                }
                else if (get_symbol_type($1) == 5){
                    gencode_function("istore ");
                    sprintf(tempbuf, "%d\n", find_stack_num_of_local_var($1));
                    gencode_function(tempbuf);
                }
            }
        }
    }
    | INC ID {
        if(lookup_symbol($2) == -1)
            print_error("Undeclared variable ", $2);
        else{
            char tempbuf[32];
            float var1[6], var2[6];
            var1[0] = get_symbol_value($2);
            var1[1] = get_symbol_type($2);
            var1[2] = 0;
            var1[3] = 1;
            var1[4] = find_stack_num_of_local_var($2);
            var1[5] = 0;

            gencode_INC_DEC(var1, 0);

            if(check_global_variable($2) == 1){
                gencode_store_global($2);
            }
            else{
                if (get_symbol_type($2) == 1){
                    gencode_function("istore ");
                    sprintf(tempbuf, "%d\n", find_stack_num_of_local_var($2));
                    gencode_function(tempbuf);
                }
                else if (get_symbol_type($2) == 2){
                    gencode_function("fstore ");
                    sprintf(tempbuf, "%d\n", find_stack_num_of_local_var($2));
                    gencode_function(tempbuf);
                }
                else if (get_symbol_type($2) == 3){
                    gencode_function("istore ");
                    sprintf(tempbuf, "%d\n", find_stack_num_of_local_var($2));
                    gencode_function(tempbuf);
                }
                else if (get_symbol_type($2) == 4){
                    gencode_function("astore ");
                    sprintf(tempbuf, "%d\n", find_stack_num_of_local_var($2));
                    gencode_function(tempbuf);
                }
                else if (get_symbol_type($2) == 5){
                    gencode_function("istore ");
                    sprintf(tempbuf, "%d\n", find_stack_num_of_local_var($2));
                    gencode_function(tempbuf);
                }
            }
        }
    }
    | DEC ID {
        if(lookup_symbol($2) == -1)
            print_error("Undeclared variable ", $2);
        else{
            char tempbuf[32];
            float var1[6], var2[6];
            var1[0] = get_symbol_value($2);
            var1[1] = get_symbol_type($2);
            var1[2] = 0;
            var1[3] = 1;
            var1[4] = find_stack_num_of_local_var($2);
            var1[5] = 0;

            gencode_INC_DEC(var1, 1);

            if(check_global_variable($2) == 1){
                gencode_store_global($2);
            }
            else{
                if (get_symbol_type($2) == 1){
                    gencode_function("istore ");
                    sprintf(tempbuf, "%d\n", find_stack_num_of_local_var($2));
                    gencode_function(tempbuf);
                }
                else if (get_symbol_type($2) == 2){
                    gencode_function("fstore ");
                    sprintf(tempbuf, "%d\n", find_stack_num_of_local_var($2));
                    gencode_function(tempbuf);
                }
                else if (get_symbol_type($2) == 3){
                    gencode_function("istore ");
                    sprintf(tempbuf, "%d\n", find_stack_num_of_local_var($2));
                    gencode_function(tempbuf);
                }
                else if (get_symbol_type($2) == 4){
                    gencode_function("astore ");
                    sprintf(tempbuf, "%d\n", find_stack_num_of_local_var($2));
                    gencode_function(tempbuf);
                }
                else if (get_symbol_type($2) == 5){
                    gencode_function("istore ");
                    sprintf(tempbuf, "%d\n", find_stack_num_of_local_var($2));
                    gencode_function(tempbuf);
                }
            }
        }
    }
    | ID INC {
        if(lookup_symbol($1) == -1)
            print_error("Undeclared variable ", $1);
        else{
            char tempbuf[32];
            float var1[6], var2[6];
            var1[0] = get_symbol_value($1);
            var1[1] = get_symbol_type($1);
            var1[2] = 0;
            var1[3] = 1;
            var1[4] = find_stack_num_of_local_var($1);
            var1[5] = 0;

            gencode_INC_DEC(var1, 0);

            if(check_global_variable($1) == 1){
                gencode_store_global($1);
            }
            else{
                if (get_symbol_type($1) == 1){
                    gencode_function("istore ");
                    sprintf(tempbuf, "%d\n", find_stack_num_of_local_var($1));
                    gencode_function(tempbuf);
                }
                else if (get_symbol_type($1) == 2){
                    gencode_function("fstore ");
                    sprintf(tempbuf, "%d\n", find_stack_num_of_local_var($1));
                    gencode_function(tempbuf);
                }
                else if (get_symbol_type($1) == 3){
                    gencode_function("istore ");
                    sprintf(tempbuf, "%d\n", find_stack_num_of_local_var($1));
                    gencode_function(tempbuf);
                }
                else if (get_symbol_type($1) == 4){
                    gencode_function("astore ");
                    sprintf(tempbuf, "%d\n", find_stack_num_of_local_var($1));
                    gencode_function(tempbuf);
                }
                else if (get_symbol_type($1) == 5){
                    gencode_function("istore ");
                    sprintf(tempbuf, "%d\n", find_stack_num_of_local_var($1));
                    gencode_function(tempbuf);
                }
            }
        }
    }
    | ID DEC{
        if(lookup_symbol($1) == -1)
            print_error("Undeclared variable ", $1);
        else{
            char tempbuf[32];
            float var1[6], var2[6];
            var1[0] = get_symbol_value($1);
            var1[1] = get_symbol_type($1);
            var1[2] = 0;
            var1[3] = 1;
            var1[4] = find_stack_num_of_local_var($1);
            var1[5] = 0;

            gencode_INC_DEC(var1, 1);

            if(check_global_variable($1) == 1){
                gencode_store_global($1);
            }
            else{
                if (get_symbol_type($1) == 1){
                    gencode_function("istore ");
                    sprintf(tempbuf, "%d\n", find_stack_num_of_local_var($1));
                    gencode_function(tempbuf);
                }
                else if (get_symbol_type($1) == 2){
                    gencode_function("fstore ");
                    sprintf(tempbuf, "%d\n", find_stack_num_of_local_var($1));
                    gencode_function(tempbuf);
                }
                else if (get_symbol_type($1) == 3){
                    gencode_function("istore ");
                    sprintf(tempbuf, "%d\n", find_stack_num_of_local_var($1));
                    gencode_function(tempbuf);
                }
                else if (get_symbol_type($1) == 4){
                    gencode_function("astore ");
                    sprintf(tempbuf, "%d\n", find_stack_num_of_local_var($1));
                    gencode_function(tempbuf);
                }
                else if (get_symbol_type($1) == 5){
                    gencode_function("istore ");
                    sprintf(tempbuf, "%d\n", find_stack_num_of_local_var($1));
                    gencode_function(tempbuf);
                }
            }
        }
    }
;

function_call
    : ID LB function_send_parameter RB {
        if(lookup_symbol($1) == -1)
            print_error("Undeclared function ", $1);
        else{
            $$[0] = 100;
            $$[1] = get_symbol_type($1);
            $$[2] = 1;  $$[3] = 0;
            $$[4] = -1; $$[5] = 0;

            if(check_fun_call_error($1) == -1){
                print_error("Function call parameter error ", $1);
            }

            gencode_function_call($1);
        }
    }
    | ID LB RB {
        if(lookup_symbol($1) == -1)
            print_error("Undeclared function ", $1);
        else{
            $$[0] = 100;
            $$[1] = get_symbol_type($1);
            $$[2] = 1;  $$[3] = 0;
            $$[4] = -1; $$[5] = 0;

            if(check_fun_call_error($1) == -1){
                print_error("Function call parameter error ", $1);
            }

            gencode_function_call($1);
        }
    }
;

function_send_parameter
    : function_send_parameter COMMA logical_stats
    {
        function_call_parameter_array[function_call_parameter_num] = $3[1];
        ++function_call_parameter_num;

        gencode_variable_load($3);
    }
    | logical_stats
    {
        function_call_parameter_array[function_call_parameter_num] = $1[1];
        ++function_call_parameter_num;

        gencode_variable_load($1);
    }
;

print_func
    : PRINT LB value RB {
        gencode_variable_load($3);
        gencdode_print_function($3);
    }
    | PRINT LB STRING_TEXT RB {
        gencode_function("ldc \"");
        gencode_function($3);
        gencode_function("\"\n");
        gencode_function("getstatic java/lang/System/out Ljava/io/PrintStream;\n");
        gencode_function("swap\n");
        gencode_function("invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V\n");
    }
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
    reset_function_call_array();

    file = fopen("compiler_hw3.j","w");

    gencode_function(".class public compiler_hw3\n"
                    ".super java/lang/Object\n"
                    );

    yyparse();
    if(syntax_error_flag == 0){

        if(buf[0] != '\0'){
            printf("%d: %s\n", yylineno+1, buf);
            ++yylineno;
        }

        if(dump_scope_flag >= 0){
            dump_symbol(dump_scope_flag);
            dump_scope_flag = -1;
        }
            
        dump_symbol(scope_num);
        printf("\nTotal lines: %d \n",yylineno);
    }

    if(syntax_error_flag == 0)
        fclose(file);

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

    fclose(file);
    remove("compiler_hw3.j");
    file_delete_flag = 1;
    if(syntax_error_flag == 1) exit(-1);
    
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
    //strncpy(temp->name, Name, strlen(Name));
    sprintf(temp->name, "%s", Name);
    temp->kind = Kind;
    temp->type = Type;
    temp->scope = scope_num;
    temp->attribute = NULL;

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
                for(int i=1; i< temp->parameter_num; ++i){
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

void set_function_parameter(char* Name){
    parse_table *temp = head;
    while(temp->next != NULL){
        temp = temp->next;
        if(strcmp(temp->name, Name) == 0){
            temp->attribute = (int*)malloc(sizeof(int)*function_parameter_num);
            temp->parameter_num = function_parameter_num;
            for(int i=0; i<function_parameter_num; ++i){
                temp->attribute[i] = function_parameter_array[i];
            }
            reset_function_array();
            return;
        }
    }  
    reset_function_array();
    return;
}

int check_fun_error_initail(char* Name){
    parse_table *temp = head;
    while(temp->next != NULL){
        temp = temp->next;
        if(strcmp(temp->name, Name) == 0){
            if(temp->parameter_num != function_parameter_num){
                reset_function_array();
                return -1;      //mean error occur
            }
            for(int i=0; i<temp->parameter_num; ++i){
                if(temp->attribute[i] != function_parameter_array[i]){
                    reset_function_array();
                    return -1;      //mean error occur
                }
            }
            return 0;
        }
    } 
    return 0; 
}

int check_fun_call_error(char* Name){
    parse_table *temp = head;
    while(temp->next != NULL){
        temp = temp->next;
        if(strcmp(temp->name, Name) == 0){
            if(temp->parameter_num != function_call_parameter_num){
                reset_function_call_array();
                printf("function parameter num error\n");
                return -1;      //mean error occur
            }
            for(int i=0; i<temp->parameter_num; ++i){
                if(temp->attribute[i] != function_call_parameter_array[i]){
                    printf("function parameter type error\n");
                    reset_function_call_array();
                    return -1;      //mean error occur
                }
            }
            reset_function_call_array();
            return 0;
        }
    } 
    reset_function_call_array();
    return 0; 
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

int get_symbol_index(char* Name){
    parse_table* temp = head;
    while(temp->next != NULL){
        temp = temp->next;
        if(strcmp(temp->name, Name) == 0 && temp->scope == scope_num){
            return temp->index;
        }
    }   
    temp = head;
    while(temp->next != NULL){
        temp = temp->next;
        if(strcmp(temp->name, Name) == 0 && temp->scope < scope_num){
             return temp->index;
        }
    }  
}

int find_stack_num_of_local_var(char* Name){
    int return_num = 0;
    parse_table* temp = head;
    while(temp->next != NULL){
        temp = temp->next;
        if(strcmp(temp->name, Name) == 0 && temp->scope <= scope_num && temp->scope > 0){
            return return_num;
        }
        if(temp->scope <= scope_num && temp->scope > 0){
            ++return_num;
        }
    }   
    return -1;
}

parse_table* get_symbol_by_index(int INDEX){
    parse_table* temp = head;
    while(temp->next != NULL){
        temp = temp->next;
        if(temp->index == INDEX){
            return temp;
        }
    }   
}

parse_table* get_function_parse_by_name(char* Name){
    parse_table* temp = head;
    while(temp->next != NULL){
        temp = temp->next;
        if(strcmp(temp->name, Name) == 0 && temp->kind == 2){
            return temp;
        }
    }  
    printf("can't find function parse by name\n");
    return NULL;
}

int check_global_variable(char* Name){
    parse_table* temp = head;
    while(temp->next != NULL){
        temp = temp->next;
        if(strcmp(temp->name, Name) == 0 && temp->scope == 0){
            return 1;
        }
    }
    return 0;
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

void reset_function_call_array(){
    for(int i=0; i<512; ++i)
        function_call_parameter_array[i] = -1;
    function_call_parameter_num = 0;
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

void gencode_function(char* output_text){
    if(file_delete_flag == 0){
            fprintf(file, output_text);
    }
}

void gencode_positive(float* var1){
    if(var1[2] == 0){
        if(var1[3] == 0){
            gencode_function("ldc ");
            char tempbuf[32];
            if(var1[1] == 1){
                sprintf(tempbuf, "%d\n", (int)var1[0]);
                gencode_function(tempbuf);
                gencode_function("ldc 1\n");
                gencode_function("imul\n");
            }  
            else if(var1[1] == 2){
                sprintf(tempbuf, "%f\n", (float)var1[0]);
                gencode_function(tempbuf);
                gencode_function("ldc 1.0\n");
                gencode_function("fmul\n");
            }
        }
        else if(var1[3] == 1){
            char tempbuf[32];
            if(var1[5] == 1){
                gencode_function("getstatic compiler_hw3/");
                parse_table *temp_parse = get_symbol_by_index(var1[4]);
                gencode_function(temp_parse->name);
                if(var1[1] == 1){
                    gencode_function(" I\n");
                    gencode_function("ldc 1\n");
                    gencode_function("imul\n");
                }  
                else if(var1[1] == 2){
                    gencode_function(" F\n");
                    gencode_function("ldc 1.0\n");
                    gencode_function("fmul\n");
                }
            }
            else if(var1[5] == 0){
                if(var1[1] == 1){
                    gencode_function("iload ");
                    sprintf(tempbuf, "%d\n", (int)var1[4]);
                    gencode_function(tempbuf);
                    gencode_function("ldc 1\n");
                    gencode_function("imul\n");
                }  
                else if(var1[1] == 2){
                    gencode_function("fload ");
                    sprintf(tempbuf, "%d\n", (int)var1[4]);
                    gencode_function(tempbuf);
                    gencode_function("ldc 1.0\n");
                    gencode_function("fmul\n");
                }
            }
        }
    }
    else if(var1[2] == 1){
        if(var1[1] == 1){
            gencode_function("ldc 1\n");
            gencode_function("imul\n");
        }
        else if(var1[1] == 2){
            gencode_function("ldc 1.0\n");
            gencode_function("fmul\n");
        }
    }
}

void gencode_negative(float* var1){
    if(var1[2] == 0){
        if(var1[3] == 0){
            gencode_function("ldc ");
            char tempbuf[32];
            if(var1[1] == 1){
                sprintf(tempbuf, "%d\n", (int)var1[0]);
                gencode_function(tempbuf);
                gencode_function("ldc -1\n");
                gencode_function("imul\n");
            }  
            else if(var1[1] == 2){
                sprintf(tempbuf, "%f\n", (float)var1[0]);
                gencode_function(tempbuf);
                gencode_function("ldc -1.0\n");
                gencode_function("fmul\n");
            }
        }
        else if(var1[3] == 1){
            char tempbuf[32];
            if(var1[5] == 1){
                gencode_function("getstatic compiler_hw3/");
                parse_table *temp_parse = get_symbol_by_index(var1[4]);
                gencode_function(temp_parse->name);
                if(var1[1] == 1){
                    gencode_function(" I\n");
                    gencode_function("ldc -1\n");
                    gencode_function("imul\n");
                }  
                else if(var1[1] == 2){
                    gencode_function(" F\n");
                    gencode_function("ldc -1.0\n");
                    gencode_function("fmul\n");
                }
            }
            else if(var1[5] == 0){
                if(var1[1] == 1){
                    gencode_function("iload ");
                    sprintf(tempbuf, "%d\n", (int)var1[4]);
                    gencode_function(tempbuf);
                    gencode_function("ldc -1\n");
                    gencode_function("imul\n");
                }  
                else if(var1[1] == 2){
                    gencode_function("fload ");
                    sprintf(tempbuf, "%d\n", (int)var1[4]);
                    gencode_function(tempbuf);
                    gencode_function("ldc -1.0\n");
                    gencode_function("fmul\n");
                }
            }
        }
    }
    else if(var1[2] == 1){
        if(var1[1] == 1){
            gencode_function("ldc -1\n");
            gencode_function("imul\n");
        }
        else if(var1[1] == 2){
            gencode_function("ldc -1.0\n");
            gencode_function("fmul\n");
        }
    }
}

void gencode_string_text(char* string_text){
    if(scope_num > 0){
        gencode_function("ldc \"");
        gencode_function(string_text);
        gencode_function("\"\n");
    }
}

void gencode_INC_DEC(float* var1, int mode){
    if(var1[2] == 0){
        if(var1[3] == 0){
            gencode_function("ldc ");
            char tempbuf[32];
            if(var1[1] == 1){
                sprintf(tempbuf, "%d\n", (int)var1[0]);
                gencode_function(tempbuf);
                gencode_function("ldc 1\n");
                if(mode == 0)
                    gencode_function("iadd\n");
                else    
                    gencode_function("isub\n");
            }  
            else if(var1[1] == 2){
                sprintf(tempbuf, "%f\n", (float)var1[0]);
                gencode_function(tempbuf);
                gencode_function("ldc 1.0\n");
                if(mode == 0)
                    gencode_function("fadd\n");
                else    
                    gencode_function("fsub\n");
            }
        }
        else if(var1[3] == 1){
            char tempbuf[32];
            if(var1[5] == 1){
                gencode_function("getstatic compiler_hw3/");
                parse_table *temp_parse = get_symbol_by_index(var1[4]);
                gencode_function(temp_parse->name);
                if(var1[1] == 1){
                    gencode_function(" I\n");
                    gencode_function("ldc 1\n");
                    if(mode == 0)
                        gencode_function("iadd\n");
                    else    
                        gencode_function("isub\n");
                }  
                else if(var1[1] == 2){
                    gencode_function(" F\n");
                    gencode_function("ldc 1.0\n");
                    if(mode == 0)
                        gencode_function("fadd\n");
                    else    
                        gencode_function("fsub\n");
                }
            }
            else if(var1[5] == 0){
                if(var1[1] == 1){
                    gencode_function("iload ");
                    sprintf(tempbuf, "%d\n", (int)var1[4]);
                    gencode_function(tempbuf);
                    gencode_function("ldc 1\n");
                    if(mode == 0)
                        gencode_function("iadd\n");
                    else    
                        gencode_function("isub\n");
                }  
                else if(var1[1] == 2){
                    gencode_function("fload ");
                    sprintf(tempbuf, "%d\n", (int)var1[4]);
                    gencode_function(tempbuf);
                    gencode_function("ldc 1.0\n");
                    if(mode == 0)
                        gencode_function("fadd\n");
                    else    
                        gencode_function("fsub\n");
                }
            }
        }
    }
    else if(var1[2] == 1){
        if(var1[1] == 1){
            gencode_function("ldc 1\n");
            if(mode == 0)
                gencode_function("iadd\n");
            else    
                gencode_function("isub\n");
        }
        else if(var1[1] == 2){
            gencode_function("ldc 1.0\n");
            if(mode == 0)
                gencode_function("fadd\n");
            else    
                gencode_function("fsub\n");
        }
    }
}

void gencode_ADD_SUB_MUL_DIV_MOD(float* var1, float* var2, int mode){
    if(var1[2] == 0){
        if(var1[3] == 0){
            gencode_function("ldc ");
            char tempbuf[32];
            if(var1[1] == 1){
                sprintf(tempbuf, "%d\n", (int)var1[0]);
                gencode_function(tempbuf);
                if(var1[1] == 1 && var2[1] == 2)
                    gencode_function("i2f\n");
            }  
            else if(var1[1] == 2){
                sprintf(tempbuf, "%f\n", (float)var1[0]);
                gencode_function(tempbuf);
            }
        }
        else if(var1[3] == 1){
            char tempbuf[32];
            if(var1[5] == 1){
                gencode_function("getstatic compiler_hw3/");
                parse_table *temp_parse = get_symbol_by_index(var1[4]);
                gencode_function(temp_parse->name);
                if(var1[1] == 1){
                    gencode_function(" I\n");
                    if(var1[1] == 1 && var2[1] == 2)
                        gencode_function("i2f\n");
                }  
                else if(var1[1] == 2){
                    gencode_function(" F\n");
                }
            }
            else if(var1[5] == 0){
                if(var1[1] == 1){
                    gencode_function("iload ");
                    sprintf(tempbuf, "%d\n", (int)var1[4]);
                    gencode_function(tempbuf);
                    if(var1[1] == 1 && var2[1] == 2)
                        gencode_function("i2f\n");
                }  
                else if(var1[1] == 2){
                    gencode_function("fload ");
                    sprintf(tempbuf, "%d\n", (int)var1[4]);
                    gencode_function(tempbuf);
                }
            }
        }
    }
    else if(var1[2] == 1){
        if(var1[1] == 1 && var2[1] == 2)
            gencode_function("i2f\n"); 
    }
    if(var2[2] == 0){
        if(var2[3] == 0){
            gencode_function("ldc ");
            char tempbuf[32];
            if(var2[1] == 1){
                sprintf(tempbuf, "%d\n", (int)var2[0]);
                gencode_function(tempbuf);
                if(var1[1] == 2 && var2[1] == 1)
                    gencode_function("i2f\n");
            }  
            else if(var2[1] == 2){
                sprintf(tempbuf, "%f\n", (float)var2[0]);
                gencode_function(tempbuf);
            }
        }
        else if(var2[3] == 1){
            char tempbuf[32];
            if(var2[5] == 1){
                gencode_function("getstatic compiler_hw3/");
                parse_table *temp_parse = get_symbol_by_index(var2[4]);
                gencode_function(temp_parse->name);
                if(var2[1] == 1){
                    gencode_function(" I\n");
                    if(var1[1] == 2 && var2[1] == 1)
                        gencode_function("i2f\n");
                }  
                else if(var2[1] == 2){
                    gencode_function(" F\n");
                }
            }
            else if(var2[5] == 0){
                if(var2[1] == 1){
                    gencode_function("iload ");
                    sprintf(tempbuf, "%d\n", (int)var2[4]);
                    gencode_function(tempbuf);
                    if(var1[1] == 2 && var2[1] == 1)
                        gencode_function("i2f\n");
                }  
                else if(var2[1] == 2){
                    gencode_function("fload ");
                    sprintf(tempbuf, "%d\n", (int)var2[4]);
                    gencode_function(tempbuf);
                }
            }
        }
    }
    else if(var2[2] == 1){
        if(var1[1] == 2 && var2[1] == 1)
            gencode_function("i2f\n"); 
    }
    if(var1[1] == 2 || var2[1] == 2){
        if(mode == 0)
            gencode_function("fadd\n");
        else if(mode == 1)
            gencode_function("fsub\n");
        else if(mode == 2)
            gencode_function("fmul\n");
        else if(mode == 3)
            gencode_function("fdiv\n");
    }
    else{
        if(mode == 0)
            gencode_function("iadd\n");
        else if(mode == 1)
            gencode_function("isub\n");
        else if(mode == 2)
            gencode_function("imul\n");
        else if(mode == 3)
            gencode_function("idiv\n");
        else if(mode == 4)
            gencode_function("irem\n");
    }
}

void gencode_variable_load(float* var1){
    if(var1[2] == 0){
        if(var1[3] == 0){
            gencode_function("ldc ");
            char tempbuf[32];
            if(var1[1] == 1 || 3 || 4){
                sprintf(tempbuf, "%d\n", (int)var1[0]);
                gencode_function(tempbuf);
            }  
            else if(var1[1] == 2){
                sprintf(tempbuf, "%f\n", (float)var1[0]);
                gencode_function(tempbuf);
            }
        }
        else if(var1[3] == 1){
            char tempbuf[32];
            if(var1[5] == 1){
                gencode_function("getstatic compiler_hw3/");
                parse_table *temp_parse = get_symbol_by_index(var1[4]);
                gencode_function(temp_parse->name);
                if(var1[1] == 1){
                    gencode_function(" I\n");
                }  
                else if(var1[1] == 2){
                    gencode_function(" F\n");
                }
                else if(var1[1] == 3){
                    gencode_function(" Z\n");
                }
                else if(var1[1] == 4){
                    gencode_function(" Ljava/lang/String;\n");
                }
                else if(var1[1] == 5){
                    gencode_function(" V\n");
                }
            }
            else if(var1[5] == 0){
                if(var1[1] == 1 || 3){
                    gencode_function("iload ");
                    sprintf(tempbuf, "%d\n", (int)var1[4]);
                    gencode_function(tempbuf);
                }  
                else if(var1[1] == 2){
                    gencode_function("fload ");
                    sprintf(tempbuf, "%d\n", (int)var1[4]);
                    gencode_function(tempbuf);
                }
                else if(var1[1] == 4){
                    gencode_function("aload ");
                    sprintf(tempbuf, "%d\n", (int)var1[4]);
                    gencode_function(tempbuf);
                }
            }
        }
    }
}

void gencode_MT_LT_MTE_LTE_EQ_NE(float* var1, float* var2, int mode){
    if(var1[2] == 0){
        if(var1[3] == 0){
            gencode_function("ldc ");
            char tempbuf[32];
            if(var1[1] == 1){
                sprintf(tempbuf, "%d\n", (int)var1[0]);
                gencode_function(tempbuf);
                if(var1[1] == 1 && var2[1] == 2)
                    gencode_function("i2f\n");
            }  
            else if(var1[1] == 2){
                sprintf(tempbuf, "%f\n", (float)var1[0]);
                gencode_function(tempbuf);
            }
        }
        else if(var1[3] == 1){
            char tempbuf[32];
            if(var1[5] == 1){
                gencode_function("getstatic compiler_hw3/");
                parse_table *temp_parse = get_symbol_by_index(var1[4]);
                gencode_function(temp_parse->name);
                if(var1[1] == 1){
                    gencode_function(" I\n");
                    if(var1[1] == 1 && var2[1] == 2)
                        gencode_function("i2f\n");
                }  
                else if(var1[1] == 2){
                    gencode_function(" F\n");
                }
            }
            else if(var1[5] == 0){
                if(var1[1] == 1){
                    gencode_function("iload ");
                    sprintf(tempbuf, "%d\n", (int)var1[4]);
                    gencode_function(tempbuf);
                    if(var1[1] == 1 && var2[1] == 2)
                        gencode_function("i2f\n");
                }  
                else if(var1[1] == 2){
                    gencode_function("fload ");
                    sprintf(tempbuf, "%d\n", (int)var1[4]);
                    gencode_function(tempbuf);
                }
            }
        }
    }
    else if(var1[2] == 1){
        if(var1[1] == 1 && var2[1] == 2)
            gencode_function("i2f\n"); 
    }
    if(var2[2] == 0){
        if(var2[3] == 0){
            gencode_function("ldc ");
            char tempbuf[32];
            if(var2[1] == 1){
                sprintf(tempbuf, "%d\n", (int)var2[0]);
                gencode_function(tempbuf);
                if(var1[1] == 2 && var2[1] == 1)
                    gencode_function("i2f\n");
            }  
            else if(var2[1] == 2){
                sprintf(tempbuf, "%f\n", (float)var2[0]);
                gencode_function(tempbuf);
            }
        }
        else if(var2[3] == 1){
            char tempbuf[32];
            if(var2[5] == 1){
                gencode_function("getstatic compiler_hw3/");
                parse_table *temp_parse = get_symbol_by_index(var2[4]);
                gencode_function(temp_parse->name);
                if(var2[1] == 1){
                    gencode_function(" I\n");
                    if(var1[1] == 2 && var2[1] == 1)
                        gencode_function("i2f\n");
                }  
                else if(var2[1] == 2){
                    gencode_function(" F\n");
                }
            }
            else if(var2[5] == 0){
                if(var2[1] == 1){
                    gencode_function("iload ");
                    sprintf(tempbuf, "%d\n", (int)var2[4]);
                    gencode_function(tempbuf);
                    if(var1[1] == 2 && var2[1] == 1)
                        gencode_function("i2f\n");
                }  
                else if(var2[1] == 2){
                    gencode_function("fload ");
                    sprintf(tempbuf, "%d\n", (int)var2[4]);
                    gencode_function(tempbuf);
                }
            }
        }
    }
    else if(var2[2] == 1){
        if(var1[1] == 2 && var2[1] == 1)
            gencode_function("i2f\n"); 
    }
    if(var1[1] == 2 || var2[1] == 2){
        gencode_function("fsub\n");
    }
    else{
        gencode_function("isub\n");
    }
    logical_stat_mode_flag = mode;
}

void gencode_function_define(char* Name){
    gencode_function(".method public static ");
    gencode_function(Name);
    if (strcmp("main", Name) == 0){
        gencode_function("([Ljava/lang/String;)V\n");
    }
    else{
        gencode_function("(");
        for(int i=0; i<function_parameter_num; ++i){
            if(function_parameter_array[i] == 1)
                gencode_function("I");
            else if(function_parameter_array[i] == 2)
                gencode_function("F");
            else if(function_parameter_array[i] == 3)
                gencode_function("Z");
            else if(function_parameter_array[i] == 4)
                gencode_function("Ljava/lang/String;");
            else if(function_parameter_array[i] == 5)
                gencode_function("V");
        }
        gencode_function(")");

        if(get_symbol_type(Name) == 1)
            gencode_function("I\n");
        else if(get_symbol_type(Name) == 2)
            gencode_function("F\n");
        else if(get_symbol_type(Name) == 3)
            gencode_function("Z\n");
        else if(get_symbol_type(Name) == 4)
            gencode_function("Ljava/lang/String;\n");
        else if(get_symbol_type(Name) == 5)
            gencode_function("V\n");
    }
    gencode_function(".limit stack 50\n");
    gencode_function(".limit locals 50\n");
}

void gencode_function_call(char* Name){
    gencode_function("invokestatic compiler_hw3/");
    gencode_function(Name);
    gencode_function("(");
    parse_table* temp_parse =  get_function_parse_by_name(Name);
    for(int i = 0; i < temp_parse->parameter_num; ++i){
        if(temp_parse->attribute[i] == 1)
            gencode_function("I");
        else if(temp_parse->attribute[i] == 2)
            gencode_function("F");
        else if(temp_parse->attribute[i] == 3)
            gencode_function("Z");
        else if(temp_parse->attribute[i] == 4)
            gencode_function("Ljava/lang/String;");
        else if(temp_parse->attribute[i] == 5)
            gencode_function("V");
    }
    gencode_function(")");
    if(temp_parse->type == 1)
        gencode_function("I\n");
    else if(temp_parse->type == 2)
        gencode_function("F\n");
    else if(temp_parse->type == 3)
        gencode_function("Z\n");
    else if(temp_parse->type == 4)
        gencode_function("Ljava/lang/String;\n");
    else if(temp_parse->type == 5)
        gencode_function("V\n");
}

void gencode_store_global(char* Name){
    gencode_function("putstatic compiler_hw3/");
    gencode_function(Name);
    if(get_symbol_type(Name) == 1)
        gencode_function(" I\n");
    else if(get_symbol_type(Name) == 2)
        gencode_function(" F\n");
    else if(get_symbol_type(Name) == 3)
        gencode_function(" Z\n");
    else if(get_symbol_type(Name) == 4)
        gencode_function(" Ljava/lang/String;\n");
    else if(get_symbol_type(Name) == 5)
        gencode_function(" V\n");
}

void gencdode_print_function(float* var1){
    gencode_function("getstatic java/lang/System/out Ljava/io/PrintStream;\n");
    gencode_function("swap\n");
    gencode_function("invokevirtual java/io/PrintStream/println(");
    if(var1[1] == 1)
        gencode_function("I)V\n");
    else if(var1[1] == 2)
        gencode_function("F)V\n");
    else if(var1[1] == 3)
        gencode_function("I)V\n");
    else if(var1[1] == 4)
        gencode_function("Ljava/lang/String;)V\n");
    else if(var1[1] == 5)
        gencode_function("I)V\n");
}