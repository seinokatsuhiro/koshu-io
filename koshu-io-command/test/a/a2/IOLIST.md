# I/O List


## 1. koshu calc.k a.k b.k

### 1.1 calc.k

~~~~~~~~~~
-*- koshu -*-

|== B : source A /x /y
~~~~~~~~~~

### 1.2 a.k

~~~~~~~~~~
-*- koshu -*-

|-- A  /x 0  /y 0
|-- A  /x 0  /y 1
|-- A  /x 1  /y 1
~~~~~~~~~~

### 1.3 b.k

~~~~~~~~~~
-*- koshu -*-

|-- A  /x 0  /y 0
|-- A  /x 2  /y 1
~~~~~~~~~~

### 1.4 Output

**koshu calc.k a.k b.k** exits successfully.

~~~~~~~~~~
** -*- koshu -*-
**
**  INPUT
**    calc.k
**    a.k
**    b.k
**
**  OUTPUT
**    <stdout>
**

|-- B  /x 0  /y 0
|-- B  /x 0  /y 1
|-- B  /x 1  /y 1
|-- B  /x 2  /y 1

*** 4 judges

**
**  SUMMARY
**       4 judges on B
**       4 judges in total
**
~~~~~~~~~~

## 2. koshu calc.k ...

### 2.1 calc.k

~~~~~~~~~~
-*- koshu -*-

|== B : source A /x /y
~~~~~~~~~~

### 2.2 a.k

~~~~~~~~~~
-*- koshu -*-

|-- A  /x 0  /y 0
|-- A  /x 0  /y 1
|-- A  /x 1  /y 1
~~~~~~~~~~

**koshu calc.k a.k** exits successfully.

~~~~~~~~~~
** -*- koshu -*-
**
**  INPUT
**    calc.k
**    a.k
**
**  OUTPUT
**    <stdout>
**

|-- B  /x 0  /y 0
|-- B  /x 0  /y 1
|-- B  /x 1  /y 1

*** 3 judges

**
**  SUMMARY
**       3 judges on B
**       3 judges in total
**
~~~~~~~~~~

### 2.3 b.k

~~~~~~~~~~
-*- koshu -*-

|-- A  /x 0  /y 0
|-- A  /x 2  /y 1
~~~~~~~~~~

**koshu calc.k b.k** exits successfully.

~~~~~~~~~~
** -*- koshu -*-
**
**  INPUT
**    calc.k
**    b.k
**
**  OUTPUT
**    <stdout>
**

|-- B  /x 0  /y 0
|-- B  /x 2  /y 1

*** 2 judges

**
**  SUMMARY
**       2 judges on B
**       2 judges in total
**
~~~~~~~~~~

