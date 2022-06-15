import ply.lex as lex
import ply.yacc as yacc

reserved={
    'mas' : 'SU',
    'menos' : 'RE',
    'es' : 'ASIGN'
}

tokens = [
    'N',
    'ID'
    ] + list(reserved.values())

t_SU    = r'mas'
t_RE   = r'menos'
t_ASIGN = r'es'

def t_N(t):
    r'[0-9]+'
    t.value = int(t.value)
    return t

def t_ID(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = reserved.get(t.value,'ID')
    return t

t_ignore = ' \t'

def t_error(t):
    print("Invalid Token:",t.value[0])
    t.lexer.skip(1)

precedence = (( 'left', 'SU', 'RE' ),)

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
            | s RE s'''
    if t[2] == 'mas':
        t[0] = t[1] + t[3]
    elif t[2] == 'menos':
        t[0] = t[1] - t[3]

def p_error(t):
    print("Syntax error in input!")


lexer = lex.lex()
parser = yacc.yacc()
res = parser.parse("2 mas 3") # the input
while True:
    try:
        data = input()
    except EOFError:
        break
    parser.parse(data)


# def t_newline(t):
#     r'\n+'
#     t.lexer.lineno += len( t.value )

# def p_s2uminus(t) :
#     's : RESTA s %prec RESTA'
#     t[0] = - t[2]

# def p_mult_div(t) :
#     '''s : s MULTIPLICA s
#             | s DIVIDE s'''

#     if t[2] == 'mult' :
#         t[0] = t[1] * t[3]
#     else :
#         if t[3] == 0 :
#             print("Can't divide by 0")
#             raise ZeroDivisionError('integer division by 0')
#         t[0] = t[1] / t[3]

# def p_s_NUM(t) : ##
#     's : N'
#     t[0] = t[1]

# def p_s_id(t): ##
#     's : ID'
#     try:
#         t[0] = variables[t[1]]
#     except LookupError:
#         print("Undefined variable '%s'" % t[1])
#         t[0] = 0

