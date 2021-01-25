OUTPUT="output.html"
echo "<!DOCTYPE html><html><head><title>\"advents\"</title></head><body>" > $OUTPUT
cat "../adventsClean/1.html" >> $OUTPUT
cat "../articlesClean/1.html" >> $OUTPUT
cat "../adventsClean/2.html" >> $OUTPUT
cat "../articlesClean/2.html" >> $OUTPUT
cat "../adventsClean/3.html" >> $OUTPUT
cat "../articlesClean/3.html" >> $OUTPUT
cat "../adventsClean/4.html" >> $OUTPUT
cat "../articlesClean/4.html" >> $OUTPUT
cat "../adventsClean/5.html" >> $OUTPUT
cat "../articlesClean/5.html" >> $OUTPUT
echo "</body></html>">> $OUTPUT
