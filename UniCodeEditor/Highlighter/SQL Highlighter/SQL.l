» Template file for the Delphi Compiler Generator (DCG)
» this file contains definitions and rules for a lexical analyzer
» to tokenize SQL source code
»
» Note: All identifiers which are lexer variables and/or definitions are case sensitive.
»       Identifiers preceeded by a "%" define lexer variables, all other define macros
»       (except keywords of course).
»
»---------- first section (global definitions) ----------
LexerName UCESQLHighlighter   » define name for the target module (unit name)
                              » as well as for the lexer class itself (see also Lexer.cod file)
Filter 'SQL script files (*.sql)|*.sql'
%CaseSensitive  FALSE         » tell DCG whether to use case sensitive keywords (allowed are
                              » T and TRUE all other values are considered as false,
                              » "CaseSensitive" might be abbrevated with "CS")
%%
»---------- keyword section ----------
» All following identifiers (until section end) are considered as keywords.
» This list will automatically be sorted.
ADD
ALGORITHM
ALL
ALTER
ANALYZE
AND
AS
ASC
ASENSITIVE
AUTO_INCREMENT
BEFORE
BEGIN     » Not mentioned in the MySQL handbook as being a reserved identifier but used for SPs, SFs.
BETWEEN
BIGINT
BINARY
BLOB
BOTH
BY
CALL
CASCADE
CASE
CHANGE
CHAR
CHARACTER
CHARSET
CHECK
COLLATE
COLUMN
COLUMNS
COMMIT
CONDITION
CONNECTION
CONSTRAINT
CONTINUE
CONVERT
CREATE
CROSS
CURRENT_DATE
CURRENT_TIME
CURRENT_TIMESTAMP
CURRENT_USER
CURSOR
DATABASE
DATABASES
DATE
DATETIME
DAY_HOUR
DAY_MICROSECOND
DAY_MINUTE
DAY_SECOND
DEC
DECIMAL
DECLARE
DEFAULT
DELAYED
DELETE
DELIMITER
DESC
DESCRIBE
DETERMINISTIC
DISTINCT
DISTINCTROW
DIV
DOUBLE
DROP
DUAL
EACH
ELSE
ELSEIF
ENCLOSED
END     » Not mentioned in the MySQL handbook as being a reserved identifier but used for SPs, SFs.
ENGINE
ENUM
EQUAL
ESCAPED
EXISTS
EXIT
EXPLAIN
FALSE
FETCH
FIELDS
FLOAT
FOR
FORCE
FOREIGN
FOUND
FROM
FULLTEXT
FUNCTION
GOTO
GRANT
GREATER
GROUP
HASH
HAVING
HIGH_PRIORITY
HOUR_MICROSECOND
HOUR_MINUTE
HOUR_SECOND
IF
IGNORE
IN
INDEX
INFILE
INNER
INOUT
INSENSITIVE
INSERT
INT
INTEGER
INTERVAL
INTO
IS
ITERATE
JOIN
KEY
KEYS
KILL
LEADING
LEAVE
LEFT
LESS
LIKE
LIMIT
LINES
LIST
LOAD
LOCALTIME
LOCALTIMESTAMP
LOCK
LONG
LONGBLOB
LONGTEXT
LOOP
LOW_PRIORITY
MATCH
MEDIUMBLOB
MEDIUMINT
MEDIUMTEXT
MIDDLEINT
MINUTE_MICROSECOND
MINUTE_SECOND
MOD
MODIFY
NATURAL
NOT
NO_WRITE_TO_BINLOG
NULL
NUMERIC
ON
OPTIMIZE
OPTION
OPTIONALLY
OR
ORDER
OUT
OUTER
OUTFILE
PARTITION
PARTITIONS
PASSWORD
PRECISION
PRIMARY
PRIVILEGES
PROCEDURE
PURGE
RANGE
READ
REAL
REFERENCES
REGEXP
RENAME
REPEAT
REPLACE
REQUIRE
RESTRICT
RETURN
RETURNS     » Not mentioned in the MySQL handbook as being a reserved identifier but used for SPs, SFs.
REVOKE
RIGHT
RLIKE
ROLLBACK
SCHEMA
SCHEMAS
SECOND_MICROSECOND
SELECT
SENSITIVE
SEPARATOR
SET
SHOW
SMALLINT
SONAME
SPATIAL
SPECIFIC
SQL
SQLEXCEPTION
SQLSTATE
SQLWARNING
SQL_BIG_RESULT
SQL_CALC_FOUND_ROWS
SQL_SMALL_RESULT
SSL
START
STARTING
STRAIGHT_JOIN
TABLE
TABLES
TEMPORARY
TERMINATED
THAN
THEN
TIMESTAMP
TINYBLOB
TINYINT
TINYTEXT
TO
TRAILING
TRIGGER
TRUE
TRANSACTION
UNDEFINED
UNDO
UNION
UNIQUE
UNLOCK
UNSIGNED
UPDATE
USAGE
USE
USING
UTC_DATE
UTC_TIME
UTC_TIMESTAMP
VALUE
VALUES
VARBINARY
VARCHAR
VARCHARACTER
VARYING
VIEW
WHEN
WHERE
WHILE
WITH
WRITE
XOR
YEAR_MONTH
ZEROFILL

%%
» ---------- definitions section ----------

%T IDENTIFIER INTEGERNUMBER FLOATNUMBER WHITESPACE STRINGCONSTANT USER_VARIABLE SYSTEM_VARIABLE
%T MLCOMMENT SLCOMMENT COMMENT_WITH_COMMAND EMBEDDED_COMMAND SYMBOL
%T KEYWORD UNKNOWN

%S Normal
%S MultilineComment
%S EmbeddedCommand

letter                          [A-Za-z_]
digit                           [0-9]
identifier                      (¤letter¤|¤digit¤)*¤letter¤(¤letter¤|¤digit¤)*
integer                         ¤digit¤+
sign                            [\+\-]
exp                             [Ee]¤sign¤?¤digit¤+
realinteger                     (¤sign¤)?¤digit¤+(((\.¤digit¤*)(¤exp¤)?)|¤exp¤)
white                           [\1-\40]
eof                             \0

%%
» ---------- rules section ----------
<Normal>¤integer¤
  SetToken(INTEGERNUMBER);

<Normal>¤realinteger¤
  SetToken(FLOATNUMBER);

<Normal>¤white¤+
  SetToken(WHITESPACE);

<Normal>¤identifier¤
  if IsKeyword then
    SetToken(KEYWORD)
  else
    SetToken(IDENTIFIER);

<Normal>`¤identifier¤`
  if IsKeyword then
    SetToken(KEYWORD)
  else
    SetToken(IDENTIFIER);

<Normal>@¤identifier¤
  SetToken(USER_VARIABLE);

<Normal>@@¤identifier¤
  SetToken(SYSTEM_VARIABLE);

<Normal>'
  repeat
    case CurrentChar of
      '''',
      CR, #0 :
        begin
          if CurrentChar = '''' then
            NextChar;
          SetToken(STRINGCONSTANT);
          Break;
        end;
      '\': // Escape character, skip this and the next one.
        NextChar;
    end;
    NextChar;
  until False;

<Normal>\"
  repeat
    case CurrentChar of
      '"',
      CR, #0 :
        begin
          if CurrentChar = '"' then
            NextChar;
          SetToken(STRINGCONSTANT);
          Break;
        end;
      '\': // Escape character, skip this and the next one.
        NextChar;
    end;
    NextChar;
  until False;

<Normal>"--"
  if CurrentChar in [CR, #0, ' '] then
  begin
    SetToken(SLCOMMENT);
    if CurrentChar = ' ' then
      repeat
        case CurrentChar of
          CR, #0:
            begin
              SetToken(SLCOMMENT);
              Break;
            end;
        end;
        NextChar;
      until False;
  end
  else
    SetToken(SYMBOL);

<Normal>"#"
  repeat
    case CurrentChar of
      CR, #0:
        begin
          SetToken(SLCOMMENT);
          Break;
        end;
      end;
    NextChar;
  until False;

<Normal>"/*"
  repeat
    case CurrentChar of
      '*':
        if Lookahead = '/' then
        begin
          // skip lookahead and break loop
          NextChar;
          NextChar;
          SetToken(MLCOMMENT);
          Break;
        end;
      #0:
        begin
          State := MultilineComment;
          SetToken(MLCOMMENT);
          Break;
        end;
      '!': // Very special syntax for MySQL: command in comment.
        begin
          State := EmbeddedCommand;
          SetToken(COMMENT_WITH_COMMAND);
          Break;
        end;
    end;
    NextChar;
  until False;

<MultilineComment>[^\*]+
  SetToken(MLCOMMENT);

<MultilineComment>\*
  begin
    SetToken(MLCOMMENT);
    if CurrentChar = '/' then
    begin
      NextChar;
      State := Normal;
    end;
  end;

<EmbeddedCommand>[^\*]*
  SetToken(EMBEDDED_COMMAND);

<EmbeddedCommand>\*
  begin
    if CurrentChar = '/' then
    begin
      SetToken(COMMENT_WITH_COMMAND);
      NextChar;
      State := Normal;
    end
    else
      SetToken(EMBEDDED_COMMAND);
  end;

<Normal>.
  SetToken(SYMBOL); // Any other char not catchd before.

¤eof¤
  SetToken(-1); // Special token to mark input end. Not really necessary since EOI is catched automatically.