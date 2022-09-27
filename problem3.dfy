predicate strictlySorted(a: array<int>) {
  forall j1, j2: int :: 0 <= j1 < a.Length && j1 < j2 < a.Length ==> a[j1] < a[j2]
}

method binarySearch(x: int, a: array<int>) returns (i: int)
  requires a != null
  requires strictlySorted(a)
  ensures 0 <= i <= a.Length
  ensures i < a.Length ==> x <= a[i]
  ensures 0 < i ==> a[i-1] < x
{
    var l := 0;
    var r := a.Length;
    var found := false;

    while (l < r && !found)
      invariant 0 <= l && l <= r && r <= a.Length
      invariant r == a.length || x == a[l] || x < a[r]
      invariant 0 >= l || a[l - 1] < x
      invariant r >= a.Length - 1 || x < a[r + 1]
      invariant found ==> 0 <= i < a.Length && a[i] == x
    {
      var m := l + (r - l) / 2;

      if (a[m] < x) {
        l := m + 1;
      } else if (a[m] > x) {
        r := m;
      } else {
        i := m;
        found := true;
      }
    }
    if (!found) {
      i := r + 1
    }
}