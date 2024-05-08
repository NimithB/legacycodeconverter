import os
import numpy as np
import pickle
from sklearn.model_selection import train_test_split

def preprocess_and_tokenize(cobol_code_samples, python_translations):
    tokenized_data = []
    for cobol_code, python_code in zip(cobol_code_samples, python_translations):
        # Basic preprocessing (e.g., removing comments, standardizing whitespace)
        preprocessed_cobol = cobol_code.strip()
        preprocessed_python = python_code.strip()

        # Tokenization (splitting by whitespace)
        cobol_tokens = preprocessed_cobol.split()
        python_tokens = preprocessed_python.split()

        # Add special tokens to mark the beginning and end of sequences
        cobol_tokens = ['<start>'] + cobol_tokens + ['<end>']
        python_tokens = ['<start>'] + python_tokens + ['<end>']

        # Store tokenized data as tuple
        tokenized_data.append((cobol_tokens, python_tokens))

    return tokenized_data


def split_data(tokenized_data, test_size=0.2):
    # Split tokenized data into training and testing sets
    cobol_data = [sample[0] for sample in tokenized_data]
    python_data = [sample[1] for sample in tokenized_data]

    cobol_train, cobol_test, python_train, python_test = train_test_split(
        cobol_data, python_data, test_size=test_size, random_state=42)

    return cobol_train, cobol_test, python_train, python_test

cobol_code_samples = [
    """IDENTIFICATION DIVISION.
PROGRAM-ID. CountDown.

DATA DIVISION.
WORKING-STORAGE SECTION.
01 COUNTER PIC 99 VALUE 10.

PROCEDURE DIVISION.
    PERFORM UNTIL COUNTER = 0
        DISPLAY "Count: " COUNTER
        SUBTRACT 1 FROM COUNTER
    END-PERFORM.
    STOP RUN.""",
    """IDENTIFICATION DIVISION.
PROGRAM-ID. MultiplicationTable.

DATA DIVISION.
WORKING-STORAGE SECTION.
01 MULTIPLIER PIC 99 VALUE 5.
01 COUNTER PIC 99 VALUE 1.
01 RESULT PIC 999.

PROCEDURE DIVISION.
    PERFORM VARYING COUNTER FROM 1 BY 1 UNTIL COUNTER > 10
        MULTIPLY MULTIPLIER BY COUNTER GIVING RESULT
        DISPLAY MULTIPLIER " x " COUNTER " = " RESULT
    END-PERFORM.
    STOP RUN.""",
    """IDENTIFICATION DIVISION.
PROGRAM-ID. SumOfNumbers.

DATA DIVISION.
WORKING-STORAGE SECTION.
01 N PIC 9(3) VALUE 100.
01 SUM PIC 9(5) VALUE 0.
01 COUNTER PIC 9(3) VALUE 1.

PROCEDURE DIVISION.
    PERFORM UNTIL COUNTER > N
        ADD COUNTER TO SUM
        ADD 1 TO COUNTER
    END-PERFORM.
    DISPLAY "Sum of first " N " numbers is: " SUM.
    STOP RUN.""",
    """IDENTIFICATION DIVISION.
PROGRAM-ID. EvenNumbers.

DATA DIVISION.
WORKING-STORAGE SECTION.
01 NUMBER PIC 9(3) VALUE 1.

PROCEDURE DIVISION.
    DISPLAY "Even numbers between 1 and 100:"
    PERFORM UNTIL NUMBER > 100
        IF NUMBER MOD 2 = 0 THEN
            DISPLAY NUMBER
        END-IF
        ADD 1 TO NUMBER
    END-PERFORM.
    STOP RUN.""",
    """IDENTIFICATION DIVISION.
PROGRAM-ID. NestedLoops.

DATA DIVISION.
WORKING-STORAGE SECTION.
01 ROW-NUMBER PIC 9 VALUE 1.
01 COL-NUMBER PIC 9.
01 RESULT PIC 9(4).

PROCEDURE DIVISION.
    DISPLAY "5x5 Multiplication Table:"
    PERFORM UNTIL ROW-NUMBER > 5
        PERFORM UNTIL COL-NUMBER > 5
            MULTIPLY ROW-NUMBER BY COL-NUMBER GIVING RESULT
            DISPLAY ROW-NUMBER " x " COL-NUMBER " = " RESULT
            ADD 1 TO COL-NUMBER
        END-PERFORM
        DISPLAY "----"
        ADD 1 TO ROW-NUMBER
        MOVE 1 TO COL-NUMBER
    END-PERFORM.
    STOP RUN.""",
    """IDENTIFICATION DIVISION.
PROGRAM-ID. EvenOddCheck.

DATA DIVISION.
WORKING-STORAGE SECTION.
01 NUMBER PIC 9(4) VALUE 13.

PROCEDURE DIVISION.
    IF NUMBER MOD 2 = 0 THEN
        DISPLAY "Number is even."
    ELSE
        DISPLAY "Number is odd."
    END-IF.
    STOP RUN.""",
    """IDENTIFICATION DIVISION.
PROGRAM-ID. BonusCalculation.

DATA DIVISION.
WORKING-STORAGE SECTION.
01 SALARY PIC 9(5) VALUE 60000.
01 BONUS PIC 9(5).

PROCEDURE DIVISION.
    IF SALARY > 50000 THEN
        COMPUTE BONUS = SALARY * 0.1
    ELSE
        COMPUTE BONUS = SALARY * 0.05
    END-IF.
    DISPLAY "Employee bonus is: $" BONUS.
    STOP RUN.""",
    """IDENTIFICATION DIVISION.
PROGRAM-ID. GradeEvaluator.

DATA DIVISION.
WORKING-STORAGE SECTION.
01 SCORE PIC 9(3) VALUE 88.
01 GRADE PIC A.

PROCEDURE DIVISION.
    IF SCORE >= 90 THEN
        MOVE 'A' TO GRADE
    ELSE IF SCORE >= 80 THEN
        MOVE 'B' TO GRADE
    ELSE IF SCORE >= 70 THEN
        MOVE 'C' TO GRADE
    ELSE IF SCORE >= 60 THEN
        MOVE 'D' TO GRADE
    ELSE
        MOVE 'F' TO GRADE
    END-IF.
    DISPLAY "The grade is: " GRADE.
    STOP RUN.""",
    """IDENTIFICATION DIVISION.
PROGRAM-ID. PasswordCheck.

DATA DIVISION.
WORKING-STORAGE SECTION.
01 PASSWORD PIC A(10) VALUE "Secret123".
01 USER-PASSWORD PIC A(10).

PROCEDURE DIVISION.
    ACCEPT USER-PASSWORD.
    IF USER-PASSWORD = PASSWORD THEN
        DISPLAY "Access granted."
    ELSE
        DISPLAY "Access denied."
    END-IF.
    STOP RUN.""",
    """IDENTIFICATION DIVISION.
PROGRAM-ID. LeapYearCheck.

DATA DIVISION.
WORKING-STORAGE SECTION.
01 YEAR PIC 9(4) VALUE 2024.

PROCEDURE DIVISION.
    IF YEAR MOD 4 = 0 THEN
        IF YEAR MOD 100 = 0 THEN
            IF YEAR MOD 400 = 0 THEN
                DISPLAY YEAR " is a leap year."
            ELSE
                DISPLAY YEAR " is not a leap year."
            END-IF
        ELSE
            DISPLAY YEAR " is a leap year."
        END-IF
    ELSE
        DISPLAY YEAR " is not a leap year."
    END-IF.
    STOP RUN.""",
    """IDENTIFICATION DIVISION.
PROGRAM-ID. AddNumbersFunction.

DATA DIVISION.
WORKING-STORAGE SECTION.
01 NUM1 PIC 99 VALUE 10.
01 NUM2 PIC 99 VALUE 20.
01 RESULT PIC 99.

PROCEDURE DIVISION.
    COMPUTE RESULT = FUNCTION ADD(NUM1, NUM2).
    DISPLAY "The sum of " NUM1 " and " NUM2 " is " RESULT.
    STOP RUN.

FUNCTION ADD.
    ACCEPT A, B.
    RETURN A + B.""",
    """IDENTIFICATION DIVISION.
PROGRAM-ID. EvenOddFunction.

DATA DIVISION.
WORKING-STORAGE SECTION.
01 NUMBER PIC 99 VALUE 13.
01 RESULT PIC X.

PROCEDURE DIVISION.
    MOVE FUNCTION EVENODD(NUMBER) TO RESULT
    DISPLAY NUMBER " is " RESULT.
    STOP RUN.

FUNCTION EVENODD.
    ACCEPT NUM.
    IF NUM MOD 2 = 0 THEN
        RETURN "even"
    ELSE
        RETURN "odd"
    END-IF.""",
    """IDENTIFICATION DIVISION.
PROGRAM-ID. FactorialFunction.

DATA DIVISION.
WORKING-STORAGE SECTION.
01 NUMBER PIC 9(2) VALUE 5.
01 RESULT PIC 9(9).

PROCEDURE DIVISION.
    COMPUTE RESULT = FUNCTION FACT(NUMBER).
    DISPLAY "Factorial of " NUMBER " is " RESULT.
    STOP RUN.

FUNCTION FACT.
    ACCEPT NUM.
    IF NUM = 0 THEN
        RETURN 1
    ELSE
        RETURN NUM * FUNCTION FACT(NUM - 1)
    END-IF.""",
    """IDENTIFICATION DIVISION.
PROGRAM-ID. StringLengthFunction.

DATA DIVISION.
WORKING-STORAGE SECTION.
01 STRING-VALUE PIC X(20) VALUE "Hello, World!".
01 LENGTH PIC 9(2).

PROCEDURE DIVISION.
    COMPUTE LENGTH = FUNCTION STRLEN(STRING-VALUE).
    DISPLAY "Length of string: " LENGTH.
    STOP RUN.

FUNCTION STRLEN.
    ACCEPT STRING.
    COMPUTE LENGTH = LENGTH OF STRING.
    RETURN LENGTH.""",
    """IDENTIFICATION DIVISION.
PROGRAM-ID. LeapYearFunction.

DATA DIVISION.
WORKING-STORAGE SECTION.
01 YEAR PIC 9(4) VALUE 2024.
01 RESULT PIC X.

PROCEDURE DIVISION.
    MOVE FUNCTION ISLEAP(YEAR) TO RESULT.
    DISPLAY YEAR " is a leap year? " RESULT.
    STOP RUN.

FUNCTION ISLEAP.
    ACCEPT YR.
    IF YR MOD 4 = 0 AND (YR MOD 100 <> 0 OR YR MOD 400 = 0) THEN
        RETURN "Yes"
    ELSE
        RETURN "No"
    END-IF.""",
    """IDENTIFICATION DIVISION.
PROGRAM-ID. StringConcatenation.

DATA DIVISION.
WORKING-STORAGE SECTION.
01 FIRST-NAME PIC X(10) VALUE "John".
01 LAST-NAME PIC X(10) VALUE "Doe".
01 FULL-NAME PIC X(20).

PROCEDURE DIVISION.
    MOVE FIRST-NAME TO FULL-NAME.
    STRING " " DELIMITED BY SIZE
           LAST-NAME DELIMITED BY SIZE
    INTO FULL-NAME.
    DISPLAY "Full Name: " FULL-NAME.
    STOP RUN.""",
    """IDENTIFICATION DIVISION.
PROGRAM-ID. StringLength.

DATA DIVISION.
WORKING-STORAGE SECTION.
01 STRING-VALUE PIC X(20) VALUE "Hello, World!".
01 LENGTH PIC 99.

PROCEDURE DIVISION.
    INSPECT STRING-VALUE TALLYING LENGTH FOR CHARACTERS BEFORE INITIAL SPACE.
    DISPLAY "Length of string: " LENGTH.
    STOP RUN.""",
    """IDENTIFICATION DIVISION.
PROGRAM-ID. StringReversal.

DATA DIVISION.
WORKING-STORAGE SECTION.
01 STRING-VALUE PIC X(10) VALUE "COBOL".
01 REVERSED-STRING PIC X(10).

PROCEDURE DIVISION.
    STRING REVERSE STRING-VALUE DELIMITED BY SIZE INTO REVERSED-STRING.
    DISPLAY "Original String: " STRING-VALUE.
    DISPLAY "Reversed String: " REVERSED-STRING.
    STOP RUN.""",
    """IDENTIFICATION DIVISION.
PROGRAM-ID. StringSearchReplace.

DATA DIVISION.
WORKING-STORAGE SECTION.
01 INPUT-STRING PIC X(50) VALUE "The quick brown fox jumps over the lazy dog.".
01 SEARCH-STRING PIC X(10) VALUE "fox".
01 REPLACEMENT-STRING PIC X(10) VALUE "cat".
01 OUTPUT-STRING PIC X(50).

PROCEDURE DIVISION.
    UNSTRING INPUT-STRING DELIMITED BY ALL SPACE
        INTO OUTPUT-STRING
    END-UNSTRING
    PERFORM VARYING I FROM 1 BY 1
        UNTIL I > FUNCTION LENGTH(INPUT-STRING)
            IF OUTPUT-STRING(I:LENGTH(SEARCH-STRING)) = SEARCH-STRING
                MOVE REPLACEMENT-STRING TO OUTPUT-STRING(I:I+LENGTH(REPLACEMENT-STRING)-1)
            END-IF
    END-PERFORM
    DISPLAY "Original String: " INPUT-STRING
    DISPLAY "Modified String: " OUTPUT-STRING
    STOP RUN.""",
    """IDENTIFICATION DIVISION.
PROGRAM-ID. StringTokenizer.

DATA DIVISION.
WORKING-STORAGE SECTION.
01 INPUT-STRING PIC X(50) VALUE "apple,banana,orange,grape,mango".
01 OUTPUT-STRING OCCURS 5 TIMES PIC X(10).
01 TOKEN PIC X(10).
01 DELIMITER PIC X VALUE ",".

PROCEDURE DIVISION.
    UNSTRING INPUT-STRING DELIMITED BY DELIMITER
        INTO OUTPUT-STRING
    END-UNSTRING
    PERFORM VARYING I FROM 1 BY 1
        UNTIL I > 5
            DISPLAY "Token " I ": " OUTPUT-STRING(I)
    END-PERFORM
    STOP RUN.""",
    """IDENTIFICATION DIVISION.
PROGRAM-ID. AdditionExample.

DATA DIVISION.
WORKING-STORAGE SECTION.
01 NUM1 PIC 9(4) VALUE 100.
01 NUM2 PIC 9(4) VALUE 200.
01 SUM PIC 9(5).

PROCEDURE DIVISION.
    ADD NUM1 TO NUM2 GIVING SUM.
    DISPLAY "The sum of " NUM1 " and " NUM2 " is " SUM.
    STOP RUN.""",
    """IDENTIFICATION DIVISION.
PROGRAM-ID. MultiplicationExample.

DATA DIVISION.
WORKING-STORAGE SECTION.
01 NUM1 PIC 9(3) VALUE 12.
01 NUM2 PIC 9(3) VALUE 5.
01 RESULT PIC 9(4).

PROCEDURE DIVISION.
    MULTIPLY NUM1 BY NUM2 GIVING RESULT.
    DISPLAY "The product of " NUM1 " and " NUM2 " is " RESULT.
    STOP RUN.""",
    """IDENTIFICATION DIVISION.
PROGRAM-ID. DivisionExample.

DATA DIVISION.
WORKING-STORAGE SECTION.
01 DIVIDEND PIC 9(3) VALUE 50.
01 DIVISOR PIC 9(3) VALUE 4.
01 QUOTIENT PIC 9(3).
01 REMAINDER PIC 9(3).

PROCEDURE DIVISION.
    DIVIDE DIVIDEND BY DIVISOR
    GIVING QUOTIENT REMAINDER.
    DISPLAY "Quotient: " QUOTIENT
    DISPLAY "Remainder: " REMAINDER
    STOP RUN.""",
    """IDENTIFICATION DIVISION.
PROGRAM-ID. SubtractionExample.

DATA DIVISION.
WORKING-STORAGE SECTION.
01 MINUEND PIC 9(4) VALUE 500.
01 SUBTRAHEND PIC 9(4) VALUE 200.
01 DIFFERENCE PIC 9(5).

PROCEDURE DIVISION.
    SUBTRACT SUBTRAHEND FROM MINUEND GIVING DIFFERENCE.
    DISPLAY "The difference between " MINUEND " and " SUBTRAHEND " is " DIFFERENCE.
    STOP RUN.""",

]

python_translations = [

"""for counter in range(10, 0, -1):
    print(f"Count: {counter}")
""",
"""
multiplier = 5
for counter in range(1, 11):
    result = multiplier * counter
    print(f"{multiplier} x {counter} = {result}")
""",
"""
n = 100
sum_of_numbers = sum(range(1, n+1))
print(f"Sum of first {n} numbers is: {sum_of_numbers}")
""",

"""

print("Even numbers between 1 and 100:")
for number in range(1, 101):
    if number % 2 == 0:
        print(number)
""",
"""
print("5x5 Multiplication Table:")
for row_number in range(1, 6):
    for col_number in range(1, 6):
        result = row_number * col_number
        print(f"{row_number} x {col_number} = {result}")
    print("----")  
""",
"""
number = 13
if number % 2 == 0:
    print("Number is even.")
else:
    print("Number is odd.")

""",
"""
salary = 60000
if salary > 50000:
    bonus = salary * 0.1
else:
    bonus = salary * 0.05
print("Employee bonus is: $", bonus)
""",
"""
score = 88

if score >= 90:
    grade = 'A'
elif score >= 80:
    grade = 'B'
elif score >= 70:
    grade = 'C'
elif score >= 60:
    grade = 'D'
else:
    grade = 'F'
print("The grade is:", grade)
""",
"""
password = "Secret123"
user_password = input("Enter password: ")

if user_password == password:
    print("Access granted.")
else:
    print("Access denied.")
""",
"""
year = 2024
if year % 4 == 0:
    if year % 100 == 0:
        if year % 400 == 0:
            print(year, "is a leap year.")
        else:
            print(year, "is not a leap year.")
    else:
        print(year, "is a leap year.")
else:
    print(year, "is not a leap year.")""",

    """
def add_numbers(num1, num2):
    return num1 + num2

num1 = 10
num2 = 20
result = add_numbers(num1, num2)
print("The sum of", num1, "and", num2, "is", result)
""",
"""
def check_even_odd(number):
    if number % 2 == 0:
        return "even"
    else:
        return "odd"

number = 13
result = check_even_odd(number)
print(number, "is", result)
""",
"""
def factorial(num):
    if num == 0:
        return 1
    else:
        return num * factorial(num - 1)

number = 5
result = factorial(number)
print("Factorial of", number, "is", result)
""",
"""
def string_length(string):
    return len(string)

string_value = "Hello, World!"
length = string_length(string_value)
print("Length of string:", length)
""",
"""
def is_leap_year(year):
    if (year % 4 == 0 and year % 100 != 0) or (year % 400 == 0):
        return "Yes"
    else:
        return "No"

year = 2024
result = is_leap_year(year)
print(year, "is a leap year?", result)
""",
"""
first_name = "John"
last_name = "Doe"
full_name = first_name + " " + last_name
print("Full Name:", full_name)
""",
"""
string_value = "Hello, World!"
length = len(string_value)
print("Length of string:", length)
""",
"""

string_value = "COBOL"
reversed_string = string_value[::-1]
print("Original String:", string_value)
print("Reversed String:", reversed_string)
""",
"""input_string = "The quick brown fox jumps over the lazy dog."
search_string = "fox"
replacement_string = "cat"
output_string = input_string.replace(search_string, replacement_string)
print("Original String:", input_string)
print("Modified String:", output_string)
""",
"""
input_string = "apple,banana,orange,grape,mango"
tokens = input_string.split(",")
for i, token in enumerate(tokens):
    print("Token", i+1, ":", token)""",

    """
num1 = 100
num2 = 200
sum = num1 + num2
print("The sum of", num1, "and", num2, "is", sum)
""",
"""

num1 = 12
num2 = 5
result = num1 * num2
print("The product of", num1, "and", num2, "is", result)
""",
"""
dividend = 50
divisor = 4
quotient = dividend // divisor  remainder = dividend % divisor
print("Quotient:", quotient)
print("Remainder:", remainder)
""",
"""
minuend = 500
subtrahend = 200
difference = minuend - subtrahend
print("The difference between", minuend, "and", subtrahend, "is", difference)"""
    ]


# Preprocess and tokenize data
tokenized_data = preprocess_and_tokenize(cobol_code_samples, python_translations)

# Split data into training and testing sets
cobol_train, cobol_test, python_train, python_test = split_data(tokenized_data)

# Save tokenized data and split datasets
with open('cobol_python_data.pkl', 'wb') as f:
    pickle.dump((cobol_train, cobol_test, python_train, python_test), f)

print("Data preprocessing and tokenization complete.")