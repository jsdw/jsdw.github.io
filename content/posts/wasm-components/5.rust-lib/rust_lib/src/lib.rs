use std::collections::HashMap;

mod bindings {
    wit_bindgen::generate!({
        inline:r#"
            package example:component@0.1.0;
    
            interface item-handling {
                variant item-error {
                    not-found,
                    other(string)
                }
    
                enum item-type {
                    file,
                    folder,
                }
    
                record item {
                    ty: item-type,
                    name: string,
                }
    
                resource items {
                    constructor();
                    get: func(name: string) -> result<item, item-error>;
                }
            }
    
            world example {
                use item-handling.{ item };
    
                // we'll want to be able to get items
                // from the host as needed.
                import set-items: func() -> list<item>;
    
                // export the item functionality
                export item-handling;
            }
        "#
    });

    // Boilerplate: `super::Component` is implementing our interfaces.
    type C = super::Component;
    export!(C);
}

// Anything exported from our component lives in an `exports` mod.
use bindings::exports::example::component as exports;
// Anything imported lives at the root.
use bindings::example::component as imports;

use exports::item_handling::{ 
    Item as ExportedItem,
    ItemError as ExportedItemError,
    ItemType as ExportedItemType
};
use imports::item_handling::{
    Item as ImportedItem,
    ItemType as ImportedItemType,
};

// Create a type which will implement our `items` resource:
struct MyItems {
    items: HashMap<String, ExportedItem>,
}

impl exports::item_handling::GuestItems for MyItems {
    // This is called when `items.constructor()` is called from the host.
    fn new() -> MyItems {
        let items = bindings::set_items();
        MyItems {
            items: items
                .into_iter()
                .map(|item| (item.name.clone(), to_exported_item(item)))
                .collect()
        }
    }

    // This is called when `items.get()` is called from the host.
    fn get(&self, name: String) -> Result<ExportedItem, ExportedItemError> {
        let item = self.items.get(&name).ok_or(ExportedItemError::NotFound)?;
        Ok(item.clone())
    }
}

// Different types are generated for exports and imports even if they
// are conceptually the same, so here we convert from import to export
// and work internally with the export types.
fn to_exported_item(item: ImportedItem) -> ExportedItem {
    ExportedItem {
        name: item.name,
        ty: match item.ty {
            ImportedItemType::File => ExportedItemType::File,
            ImportedItemType::Folder => ExportedItemType::Folder,
        }
    }
}

// Create a type to represent our top level component, which will implement
// all exported interfaces.
struct Component;

// Define any resources and methods that we export in our `item-handling` interface.
// here, we only need to define what will implement the `items` resource, since we
// don't have any methods in this interface.
impl exports::item_handling::Guest for Component {
    type Items = MyItems; 
}