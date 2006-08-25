for menu in main.rpm.* ; do
  bench=${menu#main.rpm.}
  rm bench.$bench
  cp $menu main.rpm
  for exe in ~/local/bin/typhon0.?.? ; do 
    echo computing $bench with $exe...
    echo ============================================================================== >> bench.$bench 
    echo $exe >> bench.$bench
    (time $exe | tail -n 10 ) >> bench.$bench 2>&1 
  done 
done
