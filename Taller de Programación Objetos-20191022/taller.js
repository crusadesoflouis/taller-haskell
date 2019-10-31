//Escriban acá su código.
let AnilloCero ={dato: 0};
AnilloCero.siguiente = AnilloCero;

AnilloCero.agregar = function (element) {
  let nuevo = Object.create(this);
  nuevo.dato = element;
  nuevo.siguiente = this.siguiente;
  this.siguiente = nuevo;
  return this
};
AnilloCero.toString = function(){
  let chain  = '' + this.dato;
  let current = this.siguiente;
  while (!(current === this)){
    chain += '->' + current.dato;
    current = current.siguiente;
  }
  return chain
};

// let Anillo = function(d){
//   let nuevo = Object.create(AnilloCero);
//   nuevo.dato = d;
//   nuevo.siguiente = nuevo;
//   return nuevo;
// };

let Anillo = function(n){
  Object.assign(this,AnilloCero);
  this.dato = n;
  this.siguiente = this
};


Anillo.prototype.map = function(f){
  let nuevo = new Anillo(f(this.dato));
  let prev = nuevo;
  let current = this.siguiente;
  while (!(current === this)){
    prev.agregar(f(current.dato));
    prev = prev.siguiente;
    current = current.siguiente;
  }
  return nuevo;
};

Anillo.prototype.copiar = function() {
  let identity = function(n) {return n;};
  return this.map(identity);
};

Anillo.prototype.cantidad = function() {
  let cant = 0;
  let count = function (n) {
    cant++;
    return n;
  };
  this.map(count);
  return cant;
};

Anillo.prototype.ponerAnteriores = function(){
  let prev = this;
  let current = this.siguiente;
  let ag = function (element) {
    let nuevo = Object.create(this);
    nuevo.dato = element;
    this.siguiente.anterior = nuevo;
    nuevo.siguiente = this.siguiente;
    this.siguiente = nuevo;
    nuevo.anterior = this;
    return this
  };
  while (!(current === this)){
    current.agregar = ag;
    current.anterior = prev;
    prev = prev.siguiente;
    current = current.siguiente;
  }
  this.anterior = prev;
  this.agregar = ag;
  return this;
};

let anilloCero = AnilloCero;

function calcularResultado(){
//Editen esta función para que devuelva lo que quieran ver. Pueden escribir acá sus tests, y descomentar las líneas que siguen para correr los tests predefinidos.
  let res = "";
  let anilloUno = {dato: 1};
  anilloUno.siguiente = anilloUno;
  Object.setPrototypeOf(anilloUno,anilloCero);
  res += anilloUno.agregar(3).agregar(2)+"<br />"+anilloUno.siguiente; //1 ↝ 2 ↝ 3<br />2 ↝ 3 ↝ 1
  res +="<br />"+(new Anillo(0)).agregar(2).agregar(1)+"<br />"+anilloCero.siguiente; //0 ↝ 1 ↝ 2<br />0
  res +="<br />"+(new Anillo(0)).agregar(2).agregar(1).map(e => e+1);//1 ↝ 2 ↝ 3
  res +="<br />"+(new Anillo(0)).agregar(1).map(e => e+4);//4 ↝ 5
  res +="<br />"+(new Anillo(1)).map(e => e*2);//2
  let anilloDos = new Anillo(2).agregar(2);
  let anilloCopia = anilloDos.copiar();
  anilloDos.agregar(2);
  anilloCopia.agregar(1);
  res +="<br />"+anilloDos;//2 ↝ 2 ↝ 2
  res +="<br />"+anilloCopia;//2 ↝ 1 ↝ 2
  res +="<br />"+anilloCopia;//2 ↝ 1 ↝ 2
  res +="<br />"+anilloDos.cantidad();//3
  res +="<br />"+new Anillo(0).agregar(3).agregar(2).ponerAnteriores().agregar(1).siguiente.anterior;//0 ↝ 1 ↝ 2 ↝ 3
  let anilloLetras = (new Anillo("a")).agregar("d").agregar("c");
  anilloLetras.siguiente.ponerAnteriores();
  anilloLetras.agregar("b");
  res +="<br />"+anilloLetras.siguiente.anterior.dato;//a
  anilloLetras.anterior.agregar("e");
  res +="<br />"+anilloLetras;//a ↝ b ↝ c ↝ d ↝ e
  res +="<br />"+anilloLetras.anterior.anterior.agregar('f').anterior;//a ↝ b ↝ c ↝ d ↝ e
  res +="<br />"+anilloDos.agregar(2).siguiente.anterior;//undefined
  res +="<br />"+(new Anillo(1)).agregar(2).siguiente.anterior;//undefined

  return res;
}
//ejercicio 1


//ejercicio1
// console.log('anillo cero');
// console.log(AnilloCero);
// console.log('anillo cero siguiente');
// console.log(AnilloCero.siguiente);
//
// console.log('anillo cero y 5');
// AnilloCero.agregar(5);
// console.log(AnilloCero.siguiente);
// console.log(AnilloCero.siguiente.siguiente);
// console.log(AnilloCero.siguiente.siguiente.siguiente);
// console.log('anillo cero + 5 + 3');
// AnilloCero.agregar(3);
// console.log(AnilloCero.siguiente.siguiente.siguiente);
// console.log(AnilloCero.toString());

//
// let AnilloUno = new Anillo(1);
// console.log(AnilloUno.agregar(6).toString());
//
// let anillo_map = new Anillo(2).agregar(4).agregar(8);
// let anillo_map_copia = anillo_map.copiar();
// anillo_map_copia.agregar(10);
// console.log(anillo_map.cantidad());
// console.log(anillo_map.toString());
// console.log(anillo_map_copia.cantidad());
// console.log(anillo_map_copia.toString());
//
// let anillo_ultimo = new Anillo(1).agregar(2).agregar(3).ponerAnteriores();
// console.log(anillo_ultimo.toString());
// console.log(anillo_ultimo.anterior.agregar(4).toString());
// console.log(anillo_ultimo.toString())
// console.log(anillo_ultimo.anterior.toString())
// console.log(anillo_ultimo.anterior.anterior.toString())