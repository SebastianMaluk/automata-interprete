from lib2to3.pgen2 import token
import ply.lex as lex
import ply.yacc as yacc


reserved={
    'mas' : 'SU',
    'menos' : 'RE',
    "por" : "MUL",
    "entre" : "DIV",
    "y" : "AND",
    "o" : "OR",
    "no" : "NOT",
    "si" : "IF",
    "entonces" : "THEN",
    "sino" : "ELSE",
    "mientras" : "WHILE",
    "hacer" : "DO",
    "para" : "FOR",
    "de" : "FROM",
    "a" : "TO",
    "hasta" : "UNTIL",
    'es' : "ASIGN",
    "\(" : "LPAREN",
    "\)" : "RPAREN",
    "-" : "UMINUS"

}

tokens = [
    'N',
    'ID'
    ] + list(reserved.values())

t_SU = r'mas'
t_RE = r'menos'
t_MUL = r'por'
t_DIV = r'entre'
t_AND = r'y'
t_OR = r'o'
t_NOT = r'no'
t_IF = r'si'
t_THEN = r'entonces'
t_ELSE = r'sino'
t_WHILE = r'mientras'
t_DO = r'hacer'
t_FOR = r'para'
t_FROM = r'de'
t_TO = r'a'
t_UNTIL = r'hasta'
t_ASIGN = r'es'
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_UMINUS = r'-'

t_ignore = ' \t'


def t_N(t):
    r'[0-9]+'
    t.value = int(t.value)
    return t

def t_ID(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = reserved.get(t.value,'ID')
    return t

def t_error(t):
    print("Invalid token:",t.value[0])
    t.lexer.skip(1)

precedence = (( 'left', 'SU', 'RE' ),
                ( 'left', 'MUL', 'DIV' ),
                ( 'left', 'AND', 'OR' ),
                ( 'right', 'NOT' ),
                ( 'right', 'UMINUS' ),
                ( 'right', 'IF' ),
                ( 'right', 'THEN' ),
                ( 'right', 'ELSE' ),
                ( 'right', 'WHILE' ),
                ( 'right', 'DO' ),
                ( 'right', 'FOR' ),
                ( 'right', 'FROM' ),
                ( 'right', 'TO' ),
                ( 'right', 'UNTIL' ),
                ( 'right', 'ASIGN' ),
                ('left', 'LPAREN', 'RPAREN'))

variables = {}

def p_resultado(t):
    'resultado : s'
    print(t[1])

def p_asignacion(t):
    'resultado : ID ASIGN s'
    variables[t[1]] = t[3]

def p_expr_num(t):
    's : N'
    t[0] = t[1]

def p_expr_id(t):
    's : ID'
    try:
        t[0] = variables[t[1]]
    except LookupError:
        print("Undefined variable '%s'" % t[1])
        t[0] = 0

def p_expr_op(t):
    '''s : s SU s
            | s RE s
            | s MUL s
            | s DIV s'''
    if t[2] == 'mas':
        t[0] = t[1] + t[3]
    elif t[2] == 'menos':
        t[0] = t[1] - t[3]
    elif t[2] == 'por':
        t[0] = t[1] * t[3]
    elif t[2] == 'entre':
        if t[3] == 0:
            print("Can't divide by zero")
            raise ZeroDivisionError("Division by zero")
        t[0] = t[1] / t[3]

def p_expr_and(t):
    's : s AND s'
    t[0] = t[1] and t[3]

def p_expr_or(t):
    's : s OR s'
    t[0] = t[1] or t[3]

def p_expr_not(t):
    's : NOT s %prec NOT'
    t[0] = not t[2]

def p_expr_uminus(t):
    's : UMINUS s %prec UMINUS'
    t[0] = -t[2]

def p_expr_if(t):
    's : IF s THEN s ELSE s'
    t[0] = t[2] if t[1] else t[4]

def p_expr_while(t):
    's : WHILE s DO s'
    while t[1]:
        t[0] = t[3]

def p_expr_for(t):
    's : FOR ID FROM s TO s DO s'
    for i in range(t[3], t[5]):
        variables[t[1]] = i
        t[0] = t[6]

def p_expr_until(t):
    's : UNTIL s DO s'
    while not t[1]:
        t[0] = t[3]

def p_expr_asign(t):
    's : ID ASIGN s'
    variables[t[1]] = t[3]

def p_expr_expr(t):
    's : LPAREN s RPAREN'
    t[0] = t[2]

def p_expr_num_expr(t):
    's : LPAREN N RPAREN'
    t[0] = t[2]

def p_expr_id_expr(t):
    's : LPAREN ID RPAREN'
    try:
        t[0] = variables[t[2]]
    except LookupError:
        print("Undefined variable '%s'" % t[2])
        t[0] = 0

def p_error(t):
    print("Syntax error at '%s'" % t.value)


lexer = lex.lex()
parser = yacc.yacc()
res = parser.parse("2 menos 3")
