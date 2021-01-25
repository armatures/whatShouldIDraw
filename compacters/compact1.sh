OUTPUT="output1.html"
echo "<!DOCTYPE html><html><head><title>\"advents\"</title></head><body>" > $OUTPUT
cat "../adventsClean/6.html" >> $OUTPUT
cat "../articlesClean/6.html" >> $OUTPUT
cat "../adventsClean/7.html" >> $OUTPUT
cat "../articlesClean/7.html" >> $OUTPUT
cat "../adventsClean/8.html" >> $OUTPUT
cat "../articlesClean/8.html" >> $OUTPUT
cat "../adventsClean/9.html" >> $OUTPUT
cat "../articlesClean/9.html" >> $OUTPUT
cat "../adventsClean/10.html" >> $OUTPUT
cat "../articlesClean/10.html" >> $OUTPUT
echo "</body></html>">> $OUTPUT
