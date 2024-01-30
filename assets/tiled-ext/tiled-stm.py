# Cannot get Python to work in Tiled. It seems easier/better to just write my own tilemap editor...

from tiled import *
import array

class STM( Plugin ):
    @classmethod
    def nameFilter( cls ):
        return "Ramen Binary Tilemap ( *.stm )"

    @classmethod
    def shortName( cls ):
        return "stm"

    @classmethod
    def write( cls, tileMap, fileName ):
        for i in range(tileMap.layerCount()):
            if isTileLayerAt( tileMap, i ):
                tileLayer = tileLayerAt( tileMap, i )
                with open( fileName + "_" + tileLayer.name, 'wb' ) as file:
                    file.write( "STMP" )
                    h = array( 'h' )
                    h.append( tileLayer.width() );
                    h.append( tileLayer.height() );
                    h.tofile( file )
                    a = array( 'i' )
                    for y in range( tileLayer.height() ):
                        for x in range( tileLayer.width() ):
                            if tileLayer.cellAt( x, y ).tile() != None:
                                a.append( tileLayer.cellAt( x, y ).tile().id() )
                            else:
                                a.append( -1 )
                    a.tofile( file )
        return True