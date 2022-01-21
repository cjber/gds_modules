 ------------
 -- Part A --
 ------------
 ----------------
 -- Question 1 --
 ----------------

 ---
 -- Table structure for table 'Book'
 ---

DROP TABLE IF EXISTS Book;
CREATE TABLE Book (
    isbn VARCHAR(13) NOT NULL,
    title CHAR(40) NOT NULL,
    publisher CHAR(20) NOT NULL,
    PRIMARY KEY(isbn)
);

---
-- Table structure for table 'Author'
---

DROP TABLE IF EXISTS Author;
CREATE TABLE Author (
    id INT(4) NOT NULL,
    name CHAR(20) NOT NULL,
    PRIMARY KEY(id)
);

---
-- Table structure for table 'Writes'
---

DROP TABLE IF EXISTS Writes;
CREATE TABLE Writes (
    isbn VARCHAR(13) NOT NULL,
    id INT(4) NOT NULL,
    PRIMARY KEY(isbn, id)
);

---
-- Table structure for table 'BookStore'
---

DROP TABLE IF EXISTS BookStore;
CREATE TABLE BookStore (
    bsid INT(4) NOT NULL,
    address CHAR(35) NOT NULL,
    bsName CHAR(20) NOT NULL,
    PRIMARY KEY(bsid)
);

---
-- Table structure for table 'Sells'
---

DROP TABLE IF EXISTS Sells;
CREATE TABLE Sells (
    bsid INT(4) NOT NULL,
    isbn VARCHAR(13) NOT NULL,
    PRIMARY KEY(bsid, isbn)
);

---
-- Adding Foreign Key constraints for table 'Writes'
---

ALTER TABLE Writes ADD CONSTRAINT fk_to_Book FOREIGN KEY(isbn)
REFERENCES Book(isbn) ON UPDATE CASCADE ON DELETE NO ACTION;

ALTER TABLE Writes ADD CONSTRAINT fk_to_Author FOREIGN KEY(id)
REFERENCES Author(id) ON UPDATE CASCADE ON DELETE NO ACTION;

---
-- Adding Foreign Key constraints for table 'Sells'
---

ALTER TABLE Sells ADD CONSTRAINT fk_to_BookStore FOREIGN KEY(bsid)
REFERENCES BookStore(bsid) ON UPDATE CASCADE ON DELETE NO ACTION;

ALTER TABLE Sells ADD CONSTRAINT fk_to_Book2 FOREIGN KEY(isbn)
REFERENCES Book(isbn) ON UPDATE CASCADE ON DELETE NO ACTION;

----------------
-- Question 2 --
----------------

-- a)

SELECT DISTINCT BS.address
FROM BookStore AS BS, Sells AS S, Book AS B
WHERE BS.bsid = S.bsid AND S.isbn = B.isbn AND B.title = 'Database Systems';

-- b)

SELECT DISTINCT B.title
FROM Book AS B, Writes AS W, Author AS A
WHERE B.isbn = W.isbn AND W.id = A.id AND A.name = 'Agatha Christie';

-- c)

SELECT DISTINCT B.title
FROM Book AS B, Writes AS W, Author AS A
WHERE B.isbn = W.isbn AND W.id = A.id
AND (A.name = 'Agatha Christie' AND A.name <> 'Ian Rankin');

-- d)

SELECT DISTINCT A.name
FROM Writes AS W1, Writes AS W2, Author AS A
WHERE W1.isbn = W2.isbn AND W1.id <> W2.id AND W1.id = A.id
ORDER BY A.name;

-- e)

SELECT DISTINCT A.name, COUNT(*) AS NumBooks
FROM Author AS A, Writes AS W
WHERE A.id = W.id AND 5 <= (SELECT COUNT(W.id)
                            FROM Writes AS W
                            WHERE W.id = A.id)
GROUP BY W.id
ORDER BY NumBooks DESC;

-- f)

SELECT DISTINCT BS.bsName
FROM BookStore AS BS
WHERE BS.bsid IN (SELECT DISTINCT S.bsid
                  FROM Sells AS S
                  WHERE S.isbn = ALL (SELECT W.isbn
                                      FROM Writes AS W
                                      WHERE W.id IN (SELECT A.id
                                                     FROM Author AS A
                                                     WHERE A.name = 'Agatha Christie')));      
                                                                                            
----------------
-- Question 3 --
----------------

---
-- Table structure for table 'Employee'
---

DROP TABLE IF EXISTS Employee;
CREATE TABLE Employee (
    eid INT(4) NOT NULL,
    ename CHAR(20) NOT NULL,
    age INT DEFAULT NULL,
    PRIMARY KEY(eid)
);

---
-- Table structure for table 'Department'
---

DROP TABLE IF EXISTS Department;
CREATE TABLE Department (
    did INT(4) NOT NULL,
    dname CHAR(20) NOT NULL,
    dtype CHAR(20) NOT NULL,
    address CHAR(35) NOT NULL,
    PRIMARY KEY(did)
);

---
-- Table structure for table 'WorksIn'
---

DROP TABLE IF EXISTS WorksIn;
CREATE TABLE WorksIn (
    eid INT(4) NOT NULL,
    did INT(4) NOT NULL,
    since DATE DEFAULT NULL,
    PRIMARY KEY(eid, did)
);

---
-- Table structure for table 'Product'
---

DROP TABLE IF EXISTS Product;
CREATE TABLE Product (
    pid INT(4) NOT NULL,
    pname CHAR(20) NOT NULL,
    ptype CHAR(20) NOT NULL,
    pcolour CHAR(20) DEFAULT NULL,
    PRIMARY KEY(pid)
);

---
-- Table structure for table 'Sells'
---

DROP TABLE IF EXISTS Sells;
CREATE TABLE Sells (
    did INT(4) NOT NULL,
    pid INT(4) NOT NULL,
    quantity INT DEFAULT NULL,
    PRIMARY KEY(did, pid)
);

---
-- Adding Foreign Key constraints for table 'WorksIn'
---

ALTER TABLE WorksIn ADD CONSTRAINT fk_to_Employee FOREIGN KEY(eid)
REFERENCES Employee(eid) ON UPDATE CASCADE ON DELETE NO ACTION;

ALTER TABLE WorksIn ADD CONSTRAINT fk_to_Department FOREIGN KEY(did)
REFERENCES Department(did) ON UPDATE CASCADE ON DELETE NO ACTION;

---
-- Adding Foreign Key constraints for table 'Sells'
---

ALTER TABLE Sells ADD CONSTRAINT fk_to_Product FOREIGN KEY(pid)
REFERENCES Product(pid) ON UPDATE CASCADE ON DELETE NO ACTION;

ALTER TABLE Sells ADD CONSTRAINT fk_to_Department2 FOREIGN KEY(did)
REFERENCES Department(did) ON UPDATE CASCADE ON DELETE NO ACTION;

----------------
-- Question 4 --
----------------

-- a)

SELECT DISTINCT P.pname
FROM Product AS P
WHERE P.pcolour = 'Blue';

-- b)

SELECT DISTINCT D.dname
FROM Department AS D, Product AS P, Sells AS S
WHERE D.did = S.did AND S.pid = P.pid
AND P.pcolour = 'Blue';

-- c)

SELECT DISTINCT D.dname
FROM Department AS D
WHERE D.did IN (SELECT WI.did
                    FROM WorksIn AS WI
                    WHERE WI.eid =ALL (SELECT E.eid
                                       FROM Employee AS E
                                       WHERE E.age < '40'))
AND D.did IN (SELECT S.did
              FROM Sells AS S
              WHERE S.pid IN (SELECT P.pid
                              FROM Product AS P
                              WHERE P.pcolour = 'Blue'));

-- d)

SELECT MAX(E.age), D.did
FROM Employee AS E, WorksIn AS WI, Department AS D
WHERE E.eid = WI.eid AND WI.did = D.did
GROUP BY D.did;

-- e) 

SELECT E.ename
FROM Employee AS E
WHERE E.age > ANY(SELECT age FROM Employee
                    WHERE eid IN (SELECT WI.eid
                                    FROM WorksIn AS WI
                                    WHERE WI.did IN (SELECT D.did
                                                        FROM Department AS D
                                                        WHERE D.dname = 'Central')));
 -- f)

SELECT ename FROM Employee
 WHERE eid IN (SELECT WI.eid
               FROM WorksIn AS WI
               WHERE WI.did IN (SELECT D.did
                                FROM Department AS D 
                                WHERE D.did NOT IN (SELECT WI.did
                                                    FROM WorksIn AS WI
                                                    WHERE WI.eid IN (SELECT E.eid
                                                                        FROM Employee AS E
                                                                        WHERE E.age > '40'))));        

-- g)

SELECT E.ename
FROM Employee AS E, WorksIn AS WI, Department AS D
WHERE E.eid = WI.eid AND WI.did = D.did
AND 5 <= (SELECT COUNT(S.quantity)
          FROM Sells AS S
          WHERE D.did = S.did);
