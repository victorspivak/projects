<?php       
  
 class DB_Model {                
     function delete() {   
         return Box_Item::delete();
     }                           
     function static_delete() {
         return Box_Item::static_delete();
     }   
  
 }                           
  
 class Box_Directory extends DB_Model {
     static $class_name = 'Box_Directory';
 }               
  
 class Box_Item {
     static $class_name = 'Box_Item';
     public function delete() {
		 echo self::static_delete() . "\n";
		 echo Box_Item::static_delete() . "\n";
		 echo static::static_delete() . "\n";
         return self::static_delete() . ' / ' . self::$class_name . ' / ' . Box_Item::static_delete();
         //return Box_Item::static_delete();
     }               
     public static function static_delete() {
         return get_called_class();
     }               
 }                       
  
 $folder = new Box_Directory();
  
 echo $folder->delete(); // will return "Box_Directory / Box_Item / Box_Item"
 echo "\n";              
 echo $folder->static_delete(); // will return "Box_Item"
 echo "\n";   
  
?>

