cd ~/ejs/book
./render book
cp book web/book.txt
mv web Eloquent\ JavaScript
zip -qr ../www/Eloquent\ JavaScript.zip Eloquent\ JavaScript -x Eloquent\ JavaScript/.htaccess
mv Eloquent\ JavaScript web
cd web
cp -r * ../../www
