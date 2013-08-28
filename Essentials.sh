echo "Downloading GetDeb and PlayDeb" &&
wget http://archive.getdeb.net/install_deb/getdeb-repository_0.1-1~getdeb1_all.deb http://archive.getdeb.net/install_deb/playdeb_0.3-1~getdeb1_all.deb &&
echo "Installing GetDeb" &&
sudo dpkg -i getdeb-repository_0.1-1~getdeb1_all.deb &&
echo "Installing PlayDeb" &&
sudo dpkg -i playdeb_0.3-1~getdeb1_all.deb &&
if [ $(lsb_release -cs) = "luna" ]
then
	echo "elementary OS Luna, Modifying to be Precise" &&
	sudo sed -i 's/luna/precise/g' /etc/apt/sources.list.d/getdeb.list &&
	sudo rm -f /etc/apt/sources.list.d/getdeb.list.bck &&
	sudo sed -i 's/luna/precise/g' /etc/apt/sources.list.d/playdeb.list &&
	sudo rm -f /etc/apt/sources.list.d/playdeb.list.bck
fi &&
echo "Deleting Downloads" &&
rm -f getdeb-repository_0.1-1~getdeb1_all.deb &&
rm -f playdeb_0.3-1~getdeb1_all.deb &&
echo "Enabling Partner Repositories" &&
sudo sed -i "/^# deb .*partner/ s/^# //" /etc/apt/sources.list &&
echo "Adding Personal Package Archives" &&
sudo add-apt-repository -y ppa:videolan/stable-daily && 
sudo add-apt-repository -y ppa:webupd8team/rhythmbox && 
sudo add-apt-repository -y ppa:otto-kesselgulasch/gimp && 
sudo add-apt-repository -y ppa:gnome3-team/gnome3 && 
sudo add-apt-repository -y ppa:webupd8team/java && 
sudo add-apt-repository -y ppa:webupd8team/y-ppa-manager && 
sudo add-apt-repository -y ppa:tualatrix/ppa && 
sudo add-apt-repository -y ppa:transmissionbt/ppa && 
echo "Adding Medibuntu" &&
if [ $(lsb_release -cs) = "luna" ]
then
	echo "elementary OS Luna, Installing as Precise" &&
	sudo wget --output-document=/etc/apt/sources.list.d/medibuntu.list http://www.medibuntu.org/sources.list.d/precise.list
else
	sudo wget --output-document=/etc/apt/sources.list.d/medibuntu.list http://www.medibuntu.org/sources.list.d/$(lsb_release -cs).list
fi && 
echo "Updating..." &&
sudo apt-get -qq update && 
echo "Authenticating Medibuntu" &&
sudo apt-get -y -qq --allow-unauthenticated install medibuntu-keyring && 
echo "Updating..." &&
sudo apt-get -qq update && 
echo "Upgrading..." &&
sudo apt-get upgrade && 
echo "Distribution Upgrading..." &&
sudo apt-get dist-upgrade && 
echo "Disabling Guest Session" &&
sudo /usr/lib/lightdm/lightdm-set-defaults -l false &&
echo "Installing Essentials..." &&
sudo apt-get install synaptic ubuntu-tweak vlc gimp gimp-data gimp-plugin-registry gimp-data-extras y-ppa-manager firestarter bleachbit openjdk-7-jre oracle-java7-installer flashplugin-installer unace unrar zip unzip p7zip-full p7zip-rar sharutils rar uudeview mpack lha arj cabextract file-roller non-free-codecs libxine1-ffmpeg mencoder flac faac faad sox ffmpeg2theora libmpeg2-4 uudeview libmpeg3-1 mpeg3-utils mpegdemux liba52-dev mpeg2dec vorbis-tools id3v2 mpg321 mpg123 libflac++6 ffmpeg totem-mozilla icedax lame libmad0 libjpeg-progs libdvdcss libdvdread4 libdvdnav4 w32codecs libavcodec-extra-53 libavformat-extra-53 libavutil-extra-51 libpostproc-extra-52 libswscale-extra-2 ubuntu-restricted-extras app-install-data-medibuntu apport-hooks-medibuntu ubuntu-wallpapers* &&
if [ $(getconf LONG_BIT) = "64" ]
then
	echo "64bit Detected" &&
	echo "Installing Google Chrome" &&
	wget https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb &&
	sudo dpkg -i google-chrome-stable_current_amd64.deb &&
	rm -f google-chrome-stable_current_amd64.deb
else
	echo "32bit Detected" &&
	echo "Installing Google Chrome" &&
	wget https://dl.google.com/linux/direct/google-chrome-stable_current_i386.deb &&
	sudo dpkg -i google-chrome-stable_current_i386.deb &&
	rm -f google-chrome-stable_current_i386.deb
fi &&
echo "Cleaning Up" &&
sudo apt-get -f install &&
sudo apt-get autoremove &&
sudo apt-get -y autoclean &&
sudo apt-get -y clean
