                        
                           KYLIX
                     PostgreSQL Driver   
                       RELEASE NOTES

This document contains installation and usage 
notes for the Kylix 2 PostgreSQL driver. For detailed
information on downloading and using the open-source 
PostgreSQL DBMS, see http://www.postgresql.org/
For additional information on using this and other
drivers with your dbExpress applications, see the
README and INSTALL documents in the root of your Kylix
installation directory.


CONTENTS
-------------------------------------------------------
 SUPPORTED EDITIONS
 INSTALLATION INSTRUCTIONS
 USAGE NOTES


================================
SUPPORTED EDITIONS
================================

This update applies to any edition of Kylix 2. Kylix 1 
does not support the new PostgreSQL driver functionality.

The provided driver has been tested with PostgreSQL
Client version 7.1.2 and server 7.1.2.

NOTE: This driver has been certified by Borland QA for
      the US locale only. It has not yet been certified
      for any other locales. Check online documentation
      for updates.


================================
INSTALLATION INSTRUCTIONS
================================

To install this update:

1.  Uncompress the dbx_postgres.tgz file

       tar xzvf dbx_postgres.tar.gz
       
    This will create a dbx_postgres directory that contains
    the file libsqlpg.so.1.0.

2.  Move the uncompressed libsqlpg.so.1.0 file to the 
    bin directory of your kylix2 directory.
    
3.  Change directories into the bin directory and
    create a symbolic link with the following command:
    
       ln -s libsqlpg.so.1.0 libsqlpg.so.1
       
4.  Edit the dbxdrivers file as described below.
    

Editing your dbxdrivers file
----------------------------

After installation, you must manually edit (using any
text editor) your dbxdrivers file. 
The default installation location for this file is
$HOME/.borland.

In the [Installed Drivers] section of the dbxdrivers
file, add: 

   PostgreSQL=1

In the [PostgreSQL] section, add:

   GetDriverFunc=getSQLDriverPGSQL
   LibraryName=libsqlpg.so.1
   VendorLib=libpq.so
   HostName=hostname
   Database=test
   User_Name=user
   Password=password
   BlobSize=-1
   ErrorResourceFile=./DbxPostgresErr.msg
   LocaleCode=0000

================================
USAGE NOTES
================================

Known issues include:

1. Unsupported data types: 
     -OID
     -ARRAY
     -network types
     -geometric types
   
2. MONEY and BYTEA are deprecated types for PostgreSQL. We
   recommend not using these, as known problems exist.
   
3. Clients can't pass an optional port number while establishing a
   connection to the PostgreSQL server. dbExpress components do not 
   surface the port number as a runtime property. Use the default port.




-------------------------------------------------------
Copyright (c) 2001 Borland Software Corporation. 
All rights reserved.
-------------------------------------------------------
