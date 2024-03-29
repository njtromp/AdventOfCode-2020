package nl.njtromp.adventofcode_2020;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

public class Puzzle01 {
    public static int solvePart1(Integer[] numbers) {
        Set<Integer> uniqueNumber = new HashSet<>(Arrays.asList(numbers));

        for (Integer n : numbers) {
            Integer counterPart = 2020 - n;
            if (uniqueNumber.contains(counterPart)) {
                return n * counterPart;
            }
        }

        return 0;
    }

    public static int solvePart2(Integer[] numbers) {
        Set<Integer> uniqueNumber = new HashSet<>(Arrays.asList(numbers));

        for (Integer n1 : numbers) {
            for (Integer n2 : numbers) {
                Integer counterPart = 2020 - n1 - n2;
                if (uniqueNumber.contains(counterPart)) {
                    return n1 * n2 * counterPart;
                }
            }
        }

        return 0;
    }

    public static void main(String[] args) {
        Integer[] numbers = {1974, 1902, 1356, 1724, 1550, 1870, 1436, 1945, 1640, 1766, 1508, 1802, 1495, 1837, 131, 1754, 1296, 1627, 1768, 1451, 1252, 1566, 1611, 1531, 1868, 1745, 1894, 1799, 1948, 1930, 1400, 2003, 1777, 1279, 472, 1474, 1787, 1406, 1522, 1646, 1865, 1581, 1609, 1705, 1383, 1276, 1613, 1190, 1856, 1528, 1091, 1540, 1720, 1824, 1734, 1919, 1681, 1686, 1344, 1644, 1670, 1710, 1708, 1458, 1728, 1972, 1630, 1995, 1763, 1935, 451, 1392, 1990, 14, 1893, 1437, 1632, 1933, 1887, 1975, 1453, 1897, 2005, 2008, 1959, 1716, 1635, 1619, 1994, 1674, 1942, 1817, 1825, 196, 769, 1065, 1662, 1079, 1574, 1554, 1621, 1857, 1312, 1544, 2001, 1991, 1602, 1669, 1982, 1309, 1556, 1855, 1284, 1641, 1786, 735, 1921, 1661, 1934, 1552, 1012, 1748, 1782, 1631, 1607, 1659, 1997, 1600, 1594, 1798, 1405, 1790, 1993, 1960, 1717, 999, 1687, 1771, 1977, 1809, 1884, 1795, 1639, 1565, 1299, 1643, 1700, 2002, 1823, 1369, 1572, 1657, 1683, 1966, 1606, 1792, 1756, 1936, 1718, 2009, 1711, 1461, 1638, 1645, 1914, 1963, 1546, 1846, 1737, 1788, 1589, 1860, 1830, 1905, 1571, 1989, 1780, 1878, 1767, 1776, 1727, 1582, 1769, 1040, 694, 1327, 1623, 1688, 1694, 1932, 2000, 1969, 1590, 1425, 1917, 1324, 1852, 1753, 1743, 1551};
        System.out.printf("Answer part 1: %d", "", solvePart1(numbers));
        System.out.printf("Answer part 2: %d", "", solvePart2(numbers));
    }
}
