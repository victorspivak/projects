package svl.learn.kotlin.misc

fun main(args: Array<String>) {
    val a = mutableListOf(0, 1, 2, 3, 4)

    println(a)
    while(nextSet(a, a.size)) {
        println("=====> $a")
    }
}

fun swap(a:MutableList<Int>, i:Int, j:Int) {
    println("$a   $i $j")

    val v = a[i]
    a[i] = a[j]
    a[j] = v
}

fun nextSet(a:MutableList<Int>, n:Int):Boolean {
    println("<<<<<<< $a")
    var j = n - 2
    while (j != -1 && a[j] >= a[j + 1]) j--

    if (j == -1)
        return false

    var k = n - 1
    while (a[j] >= a[k]) k--
    print("*1")
    swap(a, j, k)
    var l = j + 1
    var r = n - 1

    while (l<r) {
        print("*2")
        swap(a, l++, r--);
    }
    
    return true;
}

/*
bool NextSet(int *a, int n)
{
  int j = n - 2;
  while (j != -1 && a[j] >= a[j + 1]) j--;
  if (j == -1)
    return false; // больше перестановок нет
  int k = n - 1;
  while (a[j] >= a[k]) k--;
  swap(a, j, k);
  int l = j + 1, r = n - 1; // сортируем оставшуюся часть последовательности
  while (l<r)
    swap(a, l++, r--);
  return true;
}
void Print(int *a, int n)  // вывод перестановки
{
  static int num = 1; // номер перестановки
  cout.width(3); // ширина поля вывода номера перестановки
  cout << num++ << ": ";
  for (int i = 0; i < n; i++)
    cout << a[i] << " ";
  cout << endl;
}
int main()
{
  int n, *a;
  cout << "N = ";
  cin >> n;
  a = new int[n];
  for (int i = 0; i < n; i++)
    a[i] = i + 1;
  Print(a, n);
  while (NextSet(a, n))
    Print(a, n);
  cin.get(); cin.get();
  return 0;
}} */
