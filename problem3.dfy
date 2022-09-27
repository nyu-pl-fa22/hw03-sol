predicate strictlySorted(a: array<int>, i: int) 
  reads a
  requires 0 <= i <= a.Length
{
  forall j1, j2: int :: 0 <= j1 < i && j1 < j2 < i ==> a[j1] < a[j2]
}

method isStrictlySorted(a: array<int>) returns (res: bool)
  ensures res == strictlySorted(a, a.Length)
{
  var i := 1;

  while (i < a.Length && a[i-1] < a[i])
    invariant 0 <= i <= a.Length || a.Length == 0
    invariant i <= a.Length ==> strictlySorted(a, i)
  {
    i := i + 1;
  }
  res := i >= a.Length;
}


method binarySearch(x: int, a: array<int>) returns (i: int)
  requires strictlySorted(a, a.Length)
  ensures 0 <= i <= a.Length
  ensures i < a.Length ==> x <= a[i]
  ensures 0 < i ==> a[i-1] < x
{
    var l := 0;
    var r := a.Length;
    var found := false;

    while (l < r && !found)
      invariant 0 <= l && l <= r && r <= a.Length
      invariant r == a.Length || x < a[r]
      invariant l <= 0 || a[l - 1] < x
      invariant found ==> 0 <= i < a.Length && a[i] == x
      decreases r - l, !found
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
      i := r;
    }
}