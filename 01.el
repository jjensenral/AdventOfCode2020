;;; Advent of Code 2020
;;; 01 December 2020
;;; Find pairs of numbers that sum to 2020 and return the product
;;;
;;; jens.jensen@stfc.ac.uk
;;; Should just require plain ELISP
;;;
;;; In *scratch*, load with (load "...") where the dots are the filename
;;; run as
;;; (solve2 +input+)
;;; => 712075


(defun find-pair-if (pred data)
  "Find a pair (x . y) with x,y from list data satisfying (pred x y)"
  (catch 'found
    ;; d1 CDRs through data with x being the CAR in each loop
    (do* ((d1 data (cdr d1))
	  (x (car d1) (car d1)))
	((endp d1) nil)
      ;; Same for y
      (do* ((d2 (cdr d1) (cdr d2))
	    (y (car d2) (car d2)))
	  ((endp d2) nil)
	(when (funcall pred x y) (throw 'found (cons x y)))))))

(defun find-triple-if (pred data)
  (catch 'found
    ;; d1 CDRs through data with x being the CAR in each loop
    (do* ((d1 data (cdr d1))
	  (x (car d1) (car d1)))
	((endp d1) nil)
      ;; Same for y
      (do* ((d2 (cdr d1) (cdr d2))
	    (y (car d2) (car d2)))
	  ((endp d2) nil)
	(do* ((d3 (cdr d2) (cdr d3))
	      (z (car d3) (car d3)))
	    ((endp d3) nil)
	  (when (funcall pred x y z) (throw 'found (list x y z))))))))
  


(defun solve2 (input)
  "Find a pair in the input that sums to 2020 and return the product"
  (let ((xy (find-pair-if (lambda (x y) (= (+ x y) 2020)) input)))
    (when xy (* (car xy) (cdr xy)))))

(defun solve3 (input)
  (let ((xyz (find-triple-if (lambda (x y z) (= (+ x y z) 2020)) input)))
    (when xyz (* (car xyz) (cadr xyz) (caddr xyz)))))


(defvar +input+ '(
		  1348
		  1621
		  1500
		  1818
		  1266
		  1449
		  1880
		  1416
		  1862
		  1665
		  1588
		  1704
		  1922
		  1482
		  1679
		  1263
		  1137
		  1045
		  1405
		  1048
		  1619
		  1520
		  455
		  1142
		  1415
		  1554
		  1690
		  1886
		  1891
		  1701
		  1915
		  1521
		  1253
		  1580
		  1376
		  1564
		  1747
		  1814
		  1749
		  1485
		  1969
		  974
		  1566
		  1413
		  1451
		  1200
		  1558
		  1756
		  1910
		  1044
		  470
		  1620
		  1772
		  1066
		  1261
		  1776
		  988
		  1976
		  1834
		  1896
		  1646
		  1626
		  1300
		  1692
		  1204
		  2006
		  1265
		  1911
		  1361
		  1766
		  1750
		  2000
		  1824
		  1726
		  1672
		  651
		  1226
		  1954
		  1055
		  1999
		  1793
		  1640
		  1567
		  1040
		  1426
		  1717
		  1658
		  1864
		  1917
		  695
		  1071
		  1573
		  1897
		  1546
		  1727
		  1801
		  1259
		  1290
		  1481
		  1148
		  1332
		  1262
		  1536
		  1184
		  1821
		  1681
		  1671
		  1612
		  1678
		  1703
		  1604
		  1697
		  2003
		  1453
		  1493
		  1797
		  1180
		  1234
		  1775
		  1859
		  1388
		  1393
		  667
		  1767
		  1429
		  1990
		  1322
		  1684
		  1696
		  1565
		  1380
		  1745
		  1685
		  1189
		  1396
		  1593
		  1850
		  1722
		  1495
		  1844
		  1285
		  1483
		  1635
		  1072
		  1947
		  1109
		  1586
		  1730
		  1723
		  1246
		  1389
		  1135
		  1827
		  1531
		  1583
		  1743
		  1958
		  183
		  1323
		  1949
		  1799
		  1269
		  1379
		  1950
		  1592
		  1467
		  1052
		  1418
		  2009
		  1227
		  1254
		  1865
		  1609
		  1848
		  1653
		  1691
		  1633
		  1349
		  1104
		  1790
		  1755
		  1847
		  1598
		  1872
		  1478
		  1778
		  1952
		  1694
		  1238
		  1825
		  1508
		  1141
		  1464
		  1838
		  1292
		  1403
		  1365
		  1494
		  934
		  1235
		  )
  "Input from https://adventofcode.com/2020/day/1")

