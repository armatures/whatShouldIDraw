OUTPUT="output2.html"
echo "<!DOCTYPE html><html><head><title>\"advents\"</title></head><body>" > $OUTPUT
cat "../adventsClean/11.html" >> $OUTPUT
cat "../articlesClean/11.html" >> $OUTPUT
cat "../adventsClean/12.html" >> $OUTPUT
cat "../articlesClean/12.html" >> $OUTPUT
cat "../adventsClean/13.html" >> $OUTPUT
cat "../articlesClean/13.html" >> $OUTPUT
cat "../adventsClean/14.html" >> $OUTPUT
cat "../articlesClean/14.html" >> $OUTPUT
cat "../adventsClean/15.html" >> $OUTPUT
cat "../articlesClean/15.html" >> $OUTPUT
echo "</body></html>">> $OUTPUT
