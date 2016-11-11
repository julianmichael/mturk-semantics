#!/bin/bash

DOMAIN=$1
DIR=`dirname $0`/$DOMAIN
PW=`cat $DIR/password`

openssl pkcs12 -export \
        -in $DIR/$DOMAIN.crt \
        -inkey $DIR/$DOMAIN.key \
        -out $DIR/$DOMAIN.p12 \
        -name $DOMAIN \
        -CAfile incommon-intermediates.crt \
        -caname root \
        -passout pass:$PW

cp $DIR/$DOMAIN.p12 $DIR/../../mts/jvm/src/main/resources/$DOMAIN.p12
cp $DIR/password $DIR/../../mts/jvm/src/main/resources/$DOMAIN-keystore-password

# Import the intermediate certificates
# keytool -import -v \
#         -trustcacerts \
#         -keystore $DIR/$DOMAIN.jks \
#         -storetype JKS \
#         -storepass:env PW \
#         -alias incommon-sha1 \
#         -file $DIR/../intermediates/incommon-sha1.txt


# keytool -import -v \
#         -trustcacerts \
#         -keystore $DIR/$DOMAIN.jks \
#         -storetype JKS \
#         -storepass:env PW \
#         -alias incommon-sha2 \
#         -file $DIR/../intermediates/incommon-sha2.txt

# # Import the signed certificate into $DOMAIN.jks
# keytool -import -v \
#         -trustcacerts \
#         -keystore $DIR/$DOMAIN.jks \
#         -storetype JKS \
#         -storepass:env PW \
#         -alias $DOMAIN \
#         -file $DIR/$DOMAIN.crt

# # import_intermediate(){
# # }
# # export -f import_intermediate
# # ls $DIR/intermediates | xargs xargs -n 1 -P 10 -I {} bash -c 'import_intermediate "$@"' _ {}

# # List out the contents of $DOMAIN.jks just to confirm it.
# # If you are using Play as a TLS termination point, this is the key store you should present as the server.
# keytool -list -v \
#         -keystore $DIR/$DOMAIN.jks \
#         -storepass:env PW

# cp $DIR/$DOMAIN.jks $DIR/../../mts/jvm/src/main/resources/$DOMAIN.jks
# cp $DIR/password $DIR/../../mts/jvm/src/main/resources/$DOMAIN-keystore-password
