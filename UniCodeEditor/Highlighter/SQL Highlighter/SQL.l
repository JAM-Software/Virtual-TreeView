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
ACTION
ADD
ADDDATE
AFTER
AGAINST
AGGREGATE
ALGORITHM
ALL
ALTER
ANALYZE
AND
ANY
AS
ASC
ASCII
ASENSITIVE
ATAN
ATAN2
AUTO_INCREMENT
AVG
AVG_ROW_LENGTH
BACKUP
BDB
BEFORE
BEGIN
BENCHMARK
BERKELEYDB
BETWEEN
BIGINT
BINARY
BINLOG
BIT
BIT_AND
BIT_OR
BIT_XOR
BLOB
BOOL
BOOLEAN
BOTH
BTREE
BY
BYTE
CACHE
CALL
CASCADE
CASCADED
CAST
CHAIN
CHANGE
CHANGED
CHAR
CHARACTER
CHARSET
CHECK
CHECKSUM
CIPHER
CLIENT
CLOSE
COALESCE
COLLATE
COLLATION
COLUMN
COLUMNS
COMMENT
COMMIT
COMMITTED
COMPACT
COMPRESSED
CONCAT
CONCAT_WS
CONCURRENT
CONDITION
CONNECTION
CONSTRAINT
CONTAINS
CONTINUE
CONVERT
CONVERT_TZ
COUNT
CREATE
CROSS
CUBE
CURDATE
CURRENT_DATE
CURRENT_TIME
CURRENT_TIMESTAMP
CURRENT_USER
CURSOR
CURTIME
DATA
DATABASE
DATABASES
DATE
DATE_ADD
DATE_SUB
DATETIME
DAY
DAY_HOUR
DAY_MICROSECOND
DAY_MINUTE
DAY_SECOND
DEALLOCATE
DEC
DECIMAL
DECLARE
DECODE
DEFAULT
DEFINER
DELAY_KEY_WRITE
DELAYED
DELETE
DELIMITER
DES_DECRYPT
DES_ENCRYPT
DES_KEY_FILE
DESC
DESCRIBE
DETERMINISTIC
DIRECTORY
DISABLE
DISCARD
DISTINCT
DISTINCTROW
DIV
DO
DOUBLE
DROP
DROP
DUAL
DUMPFILE
DUPLICATE
DYNAMIC
EACH
ELSE
ELSEIF
ELT
ENABLE
ENCLOSED
ENCODE
ENCRYPT
END
ENGINE
ENGINES
ENUM
EQUAL
ERRORS
ESCAPE
ESCAPED
EVENTS
EXECUTE
EXISTS
EXIT
EXPANSION
EXPLAIN
EXPORT_SET
EXTENDED
EXTRACT
FALSE
FAST
FETCH
FIELD
FIELDS
FILE
FIRST
FIXED
FLOAT
FLOAT4
FLOAT8
FLUSH
FOR
FORCE
FOREIGN
FORMAT
FOUND
FRAC_SECOND
FROM
FROM_UNIXTIME
FULL
FULLTEXT
FUNCTION
GEOMCOLLFROMTEXT
GEOMCOLLFROMWKB
GEOMETRY
GEOMETRYCOLLECTION
GEOMETRYCOLLECTIONFROMTEXT
GEOMETRYCOLLECTIONFROMWKB
GEOMETRYFROMTEXT
GEOMETRYFROMWKB
GEOMFROMTEXT
GEOMFROMWKB
GET_FORMAT
GLOBAL
GOTO
GRANT
GRANTS
GREATER
GREATEST
GROUP
GROUP_CONCAT
GROUP_UNIQUE_USERS
HANDLER
HASH
HAVING
HELP
HIGH_PRIORITY
HOSTS
HOUR
HOUR_MICROSECOND
HOUR_MINUTE
HOUR_SECOND
IDENTIFIED
IF
IGNORE
IMPORT
IN
INDEX
INDEXES
INFILE
INNER
INNOBASE
INNODB
INOUT
INSENSITIVE
INSERT
INSERT_METHOD
INT
INT1
INT2
INT3
INT4
INT8
INTEGER
INTERVAL
INTO
INVOKER
IO_THREAD
IS
ISOLATION
ISSUER
ITERATE
JOIN
KEY
KEYS
KILL
LABEL
LANGUAGE
LAST
LAST_INSERT_ID
LEADING
LEAST
LEAVE
LEAVES
LEFT
LESS
LEVEL
LIKE
LIMIT
LINEFROMTEXT
LINEFROMWKB
LINES
LINESTRING
LINESTRINGFROMTEXT
LINESTRINGFROMWKB
LIST
LOAD
LOCAL
LOCALTIME
LOCALTIMESTAMP
LOCATE
LOCK
LOCKS
LOG
LOGS
LONG
LONGBLOB
LONGTEXT
LOOP
LOW_PRIORITY
MAKE_SET
MASTER
MASTER_CONNECT_RETRY
MASTER_HOST
MASTER_LOG_FILE
MASTER_LOG_POS
MASTER_PASSWORD
MASTER_PORT
MASTER_POS_WAIT
MASTER_SERVER_ID
MASTER_SSL
MASTER_SSL_CA
MASTER_SSL_CAPATH
MASTER_SSL_CERT
MASTER_SSL_CIPHER
MASTER_SSL_KEY
MASTER_USER
MATCH
MAX
MAX_CONNECTIONS_PER_HOUR
MAX_QUERIES_PER_HOUR
MAX_ROWS
MAX_UPDATES_PER_HOUR
MAX_USER_CONNECTIONS
MEDIUM
MEDIUMBLOB
MEDIUMINT
MEDIUMTEXT
MERGE
MICROSECOND
MID
MIDDLEINT
MIGRATE
MIN
MIN_ROWS
MINUTE
MINUTE_MICROSECOND
MINUTE_SECOND
MLINEFROMTEXT
MLINEFROMWKB
MOD
MODE
MODIFIES
MODIFY
MONTH
MPOINTFROMTEXT
MPOINTFROMWKB
MPOLYFROMTEXT
MPOLYFROMWKB
MULTILINESTRING
MULTILINESTRINGFROMTEXT
MULTILINESTRINGFROMWKB
MULTIPOINT
MULTIPOINTFROMTEXT
MULTIPOINTFROMWKB
MULTIPOLYGON
MULTIPOLYGONFROMTEXT
MULTIPOLYGONFROMWKB
MUTEX
NAME
NAMES
NATIONAL
NATURAL
NCHAR
NDB
NDBCLUSTER
NEW
NEXT
NO
NO_WRITE_TO_BINLOG
NO_WRITE_TO_BINLOG
NONE
NOT
NOW
NULL
NUMERIC
NVARCHAR
OFFSET
OLD_PASSWORD
ON
ONE
ONE_SHOT
OPEN
OPTIMIZE
OPTION
OPTIONALLY
OR
ORDER
OUT
OUTER
OUTFILE
PACK_KEYS
PARTIAL
PARTITION
PARTITIONS
PASSWORD
PHASE
POINT
POINTFROMTEXT
POINTFROMWKB
POLYFROMTEXT
POLYFROMWKB
POLYGON
POLYGONFROMTEXT
POLYGONFROMWKB
POSITION
PRECISION
PREPARE
PREV 
PRIMARY
PRIVILEGES
PROCEDURE
PROCESS
PROCESSLIST
PURGE
QUARTER
QUERY
QUICK
RAID_CHUNKS
RAID_CHUNKSIZE
RAID_TYPE
RAID0
RAND
RANGE
READ
READS
REAL
RECOVER
REDUNDANT
REFERENCES
REGEXP
RELAY_LOG_FILE
RELAY_LOG_POS
RELAY_THREAD
RELEASE
RELOAD
RENAME
REPAIR
REPEAT
REPEATABLE
REPLACE
REPLICATION
REQUIRE
RESET
RESTORE
RESTRICT
RESUME
RETURN
RETURNS
REVOKE
RIGHT
RLIKE
ROLLBACK
ROLLUP
ROUND
ROUTINE
ROW
ROW_COUNT
ROW_FORMAT
ROWS
RTREE
SAVEPOINT
SCHEMA
SCHEMAS
SECOND
SECOND_MICROSECOND
SECOND_MICROSECOND
SECURITY
SELECT
SENSITIVE
SEPARATOR
SERIAL
SERIALIZABLE
SESSION
SESSION_USER
SET
SHARE
SHOW
SHUTDOWN
SIGNED
SIMPLE
SLAVE
SMALLINT
SNAPSHOT
SOME
SONAME
SOUNDS
SPATIAL
SPECIFIC
SQL
SQL_BIG_RESULT
SQL_BUFFER_RESULT
SQL_CACHE
SQL_CALC_FOUND_ROWS
SQL_NO_CACHE
SQL_SMALL_RESULT
SQL_THREAD
SQL_TSI_DAY
SQL_TSI_FRAC_SECOND
SQL_TSI_HOUR
SQL_TSI_MINUTE
SQL_TSI_MONTH
SQL_TSI_QUARTER
SQL_TSI_SECOND
SQL_TSI_WEEK
SQL_TSI_YEAR
SQLEXCEPTION
SQLSTATE
SQLWARNING
SSL
START
STARTING
STATUS
STD
STDDEV
STDDEV_POP
STDDEV_SAMP
STOP
STORAGE
STRAIGHT_JOIN
STRING
STRIPED
SUBDATE
SUBJECT
SUBSTR
SUBSTRING
SUBSTRING_INDEX
SUM
SUPER
SUSPEND
SYSDATE
SYSTEM_USER
TABLE
TABLES
TABLESPACE
TEMPORARY
TEMPTABLE
TERMINATED
TEXT
THAN
THEN
TIME
TIMESTAMP
TIMESTAMPADD
TIMESTAMPDIFF
TINYBLOB
TINYINT
TINYTEXT
TO
TRAILING
TRANSACTION
TRIGGER
TRIGGERS
TRIM
TRUE
TRUNCATE
TYPE
TYPES
UNCOMMITTED
UNDEFINED
UNDO
UNICODE
UNION
UNIQUE
UNIQUE_USERS
UNIX_TIMESTAMP
UNKNOWN
UNLOCK
UNSIGNED
UNTIL
UPDATE
USAGE
USE
USE_FRM
USER
USER_RESOURCES
USING
UTC_DATE
UTC_TIME
UTC_TIMESTAMP
VALUE
VALUES
VAR_POP
VAR_SAMP
VARBINARY
VARCHAR
VARCHARACTER
VARIABLES
VARIANCE
VARYING
VIEW
WARNINGS
WEEK
WHEN
WHERE
WHILE
WITH
WORK
WRITE
X509
XA
XOR
XOR
YEAR
YEAR_MONTH
YEARWEEK
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