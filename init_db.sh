PGPASSWORD=$(cat /home/phil/Documents/web/chandelorean/.dbpassword) psql -h 192.168.4.2 chan_archives admin -f /home/phil/Documents/web/chandelorean/sql/initialize.sql
