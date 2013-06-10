package basic

func Abs (value int) int {
	if value < 0 {
		return -value
	}

	return value
}

func Sum(start, end int) (sum int) {
	for i := start; i < end; i++ {
		sum += i
	}

	return
}

func CountChars(str string) map[rune]int {
	stats := make(map[rune]int)
	for _, c := range str {
		if cnt,found := stats[c]; found  {
			stats[c] = cnt + 1
		} else {
			stats[c] = 1
		}
	}

	return stats
}
