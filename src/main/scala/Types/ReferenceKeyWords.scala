package Types

final case class ReferenceKeyWords(
                                    // Lando System and SysML Packages
                                    System: String = "XXXXXXXX",
                                    // Lando SubSystem and SysML Part
                                    SubSystem: String = "XXXXXXXX",
                                    // Lando Component and SysML Item
                                    Component: String = "XXXXXXXX",
                                    // SysML Use case and Lando Scenario
                                    Scenario: String = "XXXXXXXX",
                                    // SysML and Lando Requirement and Cryptol Properties
                                    Requirement: String = "XXXXXXXX",
                                    // Lando Event, SysML Action and Cryptol Functions
                                    Event: String = "XXXXXXXX",
                                    //Lando Relation and SysML Connections
                                    Connection: String = "XXXXXXXX",
                                    //Imports
                                    Import: String = "XXXXXXXX",
                                    // SysML View
                                    View: String = "XXXXXXXX",
                                    // SysML View
                                    ViewPoint: String = "XXXXXXXX",
                                    //SysML Attribute
                                    Attribute: String = "XXXXXXXX",
                                    // Cryptol Type
                                    Type: String = "XXXXXXXX",
                                  )