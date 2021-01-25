OUTPUT="output4.html"
echo "<!DOCTYPE html><html><head><title>\"advents\"</title></head><body>" > $OUTPUT
cat "../adventsClean/21.html" >> $OUTPUT
cat "../articlesClean/21.html" >> $OUTPUT
cat "../adventsClean/22.html" >> $OUTPUT
cat "../articlesClean/22.html" >> $OUTPUT
cat "../adventsClean/23.html" >> $OUTPUT
cat "../articlesClean/23.html" >> $OUTPUT
cat "../adventsClean/24.html" >> $OUTPUT
cat "../articlesClean/24.html" >> $OUTPUT
cat "../adventsClean/25.html" >> $OUTPUT
cat "../articlesClean/25.html" >> $OUTPUT
echo "</body></html>">> $OUTPUT
