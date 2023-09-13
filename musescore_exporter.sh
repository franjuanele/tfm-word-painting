echo "##############################"
echo "##### MUSESCORE EXPORTER #####"
echo "##############################"
echo
echo

# Exporta usando MuseScore

if [[ $3 = svg ]]
then
	mscore -T 0 -o $2 $1
else
	mscore -o $2 $1
fi

exit 0
