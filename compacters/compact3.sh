OUTPUT="output3.html"
echo "<!DOCTYPE html><html><head><title>\"advents\"</title></head><body>" > $OUTPUT
cat "../adventsClean/16.html" >> $OUTPUT
cat "../articlesClean/16.html" >> $OUTPUT
cat "../adventsClean/17.html" >> $OUTPUT
cat "../articlesClean/17.html" >> $OUTPUT
cat "../adventsClean/18.html" >> $OUTPUT
cat "../articlesClean/18.html" >> $OUTPUT
cat "../adventsClean/19.html" >> $OUTPUT
cat "../articlesClean/19.html" >> $OUTPUT
cat "../adventsClean/20.html" >> $OUTPUT
cat "../articlesClean/20.html" >> $OUTPUT
echo "</body></html>">> $OUTPUT
