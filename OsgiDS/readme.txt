--------------------------------------------------------------------------------------------------------------
java -Xdebug -Xrunjdwp:transport=dt_socket,server=y,suspend=n,address=5005 -Dequinox.ds.debug=true -Dequinox.ds.print=true -jar D:\Tools\org\equinox-SDK-3.7\plugins\org.eclipse.osgi_3.7.0.v20110613.jar -console
java -Xdebug -Xrunjdwp:transport=dt_socket,server=y,suspend=n,address=5005 -jar D:\Tools\org\equinox-SDK-3.7\plugins\org.eclipse.osgi_3.7.0.v20110613.jar -console
java -jar D:\Tools\org\equinox-SDK-3.7\plugins\org.eclipse.osgi_3.7.0.v20110613.jar -console

install file:D:\Tools\org\equinox-SDK-3.7\plugins\org.eclipse.equinox.ds_1.3.0.v20110502.jar
install file:D:\Tools\org\equinox-SDK-3.7\plugins\org.eclipse.equinox.util_1.0.300.v20110502.jar
install file:D:\Tools\org\equinox-SDK-3.7\plugins\org.eclipse.osgi.services_3.3.0.v20110513.jar
install file:OsgiDS.jar
install file:ComparatorBundle.jar

--------------------------------------------------------------------------------------------------------------
java  -Dequinox.ds.debug=true -Dequinox.ds.print=true -jar /home/vspivak/tools/lib/equinox/plugins/org.eclipse.osgi_3.7.1.R37x_v20110808-1106.jar -console

install file:/home/vspivak/tools/lib/equinox/plugins/org.eclipse.equinox.ds_1.3.1.R37x_v20110701.jar
install file:/home/vspivak/tools/lib/equinox/plugins/org.eclipse.equinox.util_1.0.300.v20110502.jar
install file:/home/vspivak/tools/lib/equinox/plugins/org.eclipse.osgi.services_3.3.0.v20110513.jar

--------------------------------------------------------------------------------------------------------------
install file:ComparatorBundle.jar
install file:LogAPI.jar
install file:ConsoleLogger.jar
install file:NiceConsoleLogger.jar
install file:OsgiDS.jar

