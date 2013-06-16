/*******************************************************************************
 * Copyright 2010 Maxime Lévesque
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 ***************************************************************************** */
package org.squeryl.dsl

import org.squeryl.{ForeignKeyDeclaration, Table, Query, KeyedEntity, Session}
import collection.mutable.{HashMap, ArrayBuffer, Buffer}
import org.squeryl.KeyedEntityDef

trait Relation[L,R] {
  
  def leftTable: Table[L]

  def rightTable: Table[R]
}

trait OneToManyRelation[O,M] extends Relation[O,M] {

  def foreignKeyDeclaration: ForeignKeyDeclaration

  def left(leftSide: O): OneToMany[M]

  def leftStateful(leftSide: O) = new StatefulOneToMany[M](left(leftSide))

  def right(rightSide: M): ManyToOne[O]

  def rightStateful(rightSide: M) = new StatefulManyToOne[O](right(rightSide))
}

class StatefulOneToMany[M](val relation: OneToMany[M]) extends Iterable[M] {

  private var _refreshed = false
  private val _buffer = new ArrayBuffer[M]

  def refresh(implicit cs: Session) = {
    _buffer.clear
    for(m <- relation.iterator.toSeq)
      _buffer.append(m)
    _refreshed = true
    this
  }

  def iterator = {
    require(_refreshed, "iterator called before relationship was refreshed")
    _buffer.iterator
  }

  def associate(m: M)(implicit cs: Session) = {
    if (!_refreshed) refresh
    relation.associate(m)
    _buffer.append(m)
    m
  }

  def deleteAll(implicit cs: Session): Int = {
    val r = relation.deleteAll
    _buffer.clear
    _refreshed = true
    r
  }
}

class StatefulManyToOne[O](val relation: ManyToOne[O]) {

  private var _refreshed = false
  private var _one: Option[O] = None

  def refresh(implicit cs: Session) = {
    _one = relation.iterator.toSeq.headOption
    _refreshed = true
    this
  }

  def one = {
    require(_refreshed, "one called before relationship was refreshed")
    _one
  }

  def assign(o: O)(implicit cs: Session) = {
    relation.assign(o)
    _one = Some(o)
    _refreshed = true
    o
  }

  def delete(implicit cs: Session) = {
    val b = relation.delete
    _one = None
    _refreshed = true
    b
  }
}

trait ManyToManyRelation[L, R, A] extends Relation[L,R] {
  self: Table[A] =>

  def thisTable: Table[A]

  def leftForeignKeyDeclaration: ForeignKeyDeclaration

  def rightForeignKeyDeclaration: ForeignKeyDeclaration

  def left(leftSide: L): ManyToMany[R,A]

  def leftStateful(leftSide: L) = new StatefulManyToMany[R,A](left(leftSide))

  def right(rightSide: R): ManyToMany[L,A]

  def rightStateful(rightSide: R) = new StatefulManyToMany[L,A](right(rightSide))
}


/**
 * This trait is what is referred by both the left and right side of a manyToMany relation.
 * Type parameters are :
 *   O: the type at the "other" side of the relation
 *   A: the association type i.e. the entity in the "middle" of the relation
 *
 *  Object mapping to the "middle" entity are called "association objects"
 *
 * this trait extends Query[O] and can be queried against like a normal query.
 *
 * Note that this trait is used on both "left" and "right" sides of the relation,
 * but in a given relation  
 */
trait ManyToMany[O,A] extends Query[O] {

  def kedL: KeyedEntityDef[O,_]
  
  /**
   * @param a: the association object
   * 
   * Sets the foreign keys of the association object to the primary keys of the left and right side,
   * this method does not update the database, changes to the association object must be done for
   * the operation to be persisted. Alternatively the method 'associate(o, a)' will call this assign(o, a)
   * and persist the changes.
   *
   * @return the 'a' parameter is returned
   */
  def assign(o: O, a: A)(implicit cs: Session): A

  /**
   * @param a: the association object
   *
   * Calls assign(o,a) and persists the changes the database, by inserting or updating 'a', depending
   * on if it has been persisted or not.
   *
   * @return the 'a' parameter is returned
   */
  def associate(o: O, a: A)(implicit cs: Session): A

  /**
   * Creates a new association object 'a' and calls assign(o,a)
   */
  def assign(o: O)(implicit cs: Session): A

  /**
   * Creates a new association object 'a' and calls associate(o,a)
   *
   * Note that this method will fail if the association object has NOT NULL constraint fields appart from the
   * foreign keys in the relations
   *  
   */
  def associate(o: O)(implicit cs: Session): A

  /**
   * Causes the deletion of the 'Association object' between this side and the other side
   * of the relation.
   * @return true if 'o' was associated (if an association object existed between 'this' and 'o') false otherwise
   */

  def dissociate(o: O)(implicit cs: Session): Boolean

  /**
   *  Deletes all "associations" relating this "side" to the other
   */
  def dissociateAll(implicit cs: Session): Int

  /**
   * a Query returning all of this member's association entries 
   */
  def associations: Query[A]

  /**
   * @return a Query of Tuple2 containing all objects on the 'other side' along with their association object
   */
  def associationMap: Query[(O,A)]
}


class StatefulManyToMany[O,A](val relation: ManyToMany[O,A]) extends Iterable[O] {
  
  private var _refreshed = false
  private val _map = new HashMap[O,A]

  def refresh(implicit cs: Session) = {
    _map.clear
    for(e <- relation.associationMap.iterator.toSeq)
      _map.put(e._1, e._2)
    _refreshed = true
    this
  }

  def iterator = {
    require(_refreshed, "iterator called before relationship was refreshed")
    _map.keysIterator
  }

  def associate(o: O, a: A)(implicit cs: Session) = {
    if (!_refreshed) refresh
    relation.associate(o, a)
    _map.put(o, a)
    a
  }

  def associate(o: O)(implicit cs: Session): A = {
    if (!_refreshed) refresh
    val a = relation.associate(o)
    _map.put(o, a)
    a
  }

  def dissociate(o: O)(implicit cs: Session): Boolean = {
    if (!_refreshed) refresh
    val b1 = relation.dissociate(o)
    val b2 = _map.remove(o) != None
    assert(b1 == b2,
      'MutableManyToMany + " out of sync " + o.asInstanceOf[AnyRef].getClass.getName +" with id=" +
      relation.kedL.getId(o) + (if(b1) "" else "does not") + " exist in the db, and cached collection says the opposite")
    b1
  }

  def dissociateAll(implicit cs: Session): Int = {
    val r = relation.dissociateAll
    _map.clear
    _refreshed = true
    r
  }

  def associations: Iterable[A] = {
    require(_refreshed, "associations called before relationship was refreshed")
    _map.values.toSeq
  }
}


trait OneToMany[M] extends Query[M] {

  /**
   * @param the object on the 'many side' to be associated with this
   *
   *  Sets the foreign key of 'm' to refer to the primary key of the 'one' instance
   *
   * @return the 'm' parameter is returned
   */
  def assign(m: M)(implicit cs: Session): M

  /**
   * Calls 'assign(m)' and persists the changes the database, by inserting or updating 'm', depending
   * on if it has been persisted or not.
   *
   * @return the 'm' parameter is returned
   */
  def associate(m: M)(implicit cs: Session): M
  
  def deleteAll(implicit cs: Session): Int
}

trait ManyToOne[O] extends Query[O] {

  /**
   * Assigns the foreign key with the value of the 'one' primary ky
   *
   * @return the 'one' parameter is returned
   */
  def assign(one: O)(implicit cs: Session): O

  def delete(implicit cs: Session): Boolean
}
