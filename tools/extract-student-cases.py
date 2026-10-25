#!/usr/bin/env python3
"""Extract only the tables from the 20 student case workbooks.

Requires openpyxl. The source directory must directly contain the student XLSX
files. Source files are opened read-only. Keep the extraction and full trace
local; prepare-snapshot.py supplies the confirmed synthetic-data declaration
and the reviewed labels used by the public app.
"""
from pathlib import Path
from openpyxl import load_workbook
import argparse, hashlib, json, re, unicodedata

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('--source', required=True, type=Path,
                    help='Directory containing the 20 student case XLSX files')
parser.add_argument('--output', required=True, type=Path,
                    help='Local extraction JSON to write')
parser.add_argument('--trace', required=True, type=Path,
                    help='Local provenance JSON to write')
args = parser.parse_args()
SOURCE = args.source.expanduser().resolve()
if not SOURCE.is_dir():
    parser.error('--source must be an existing directory')

TITLES = {
 '01':'Cobertura del Examen de Medicina Preventiva',
 '02':'Salud mental de trabajadores',
 '03':'Formación en salud pública en estudiantes universitarios',
 '04':'Salud mental en estudiantes universitarios',
 '05':'Automedicación y uso de inteligencia artificial',
 '06.1':'Sífilis congénita','06.2':'Sífilis adquirida','07':'VIH','08':'Hidatidosis',
 '09':'Gestión de listas de espera','10':'Programa PESPI y salud intercultural',
 '11':'Control de niño sano a los 12 meses','12':'Salud sexual y reproductiva',
 '13':'Obesidad en la población adulta','14':'Diabetes mellitus tipo 2 y control metabólico',
 '15':'Discapacidad visual e inclusión','16':'Parto prematuro y riesgo obstétrico',
 '17':'Desarrollo psicomotor','18':'Sobrecarga del cuidador en cuidados paliativos',
 '19':'Adherencia a la lactancia materna',
}
# Source column indices. Numerical codes for categories remain categorical.
NUMERIC = {'01':{1,4,8},'02':{1,3,9},'03':{1,4,5,6},'04':{1,4},
 '05':{1,3,4,5},'06.1':{4,6},'06.2':{3,6,8},'07':{3,8},'08':{7},
 '09':{1,2,3},'10':{1,2,3},'11':{1,2,3},'12':{1,2,3},'13':{1,2,3},
 '14':{1,2,3},'15':{1,2,3},'16':{1,2,3},'17':{1,2,3},'18':{1,2,3},'19':{1,2,3}}
ORDINAL = {'01':{5},'02':{10},'03':{9},'04':{3,7},'05':{10},'06.1':set(),'06.2':{4},
 '07':{4},'08':set(),'09':{8},'10':{8},'11':{8},'12':{8},'13':{8},'14':{8},
 '15':{8},'16':{8},'17':{8},'18':{8},'19':{8}}
NOTES = {
 '01':['La fuente mide interés por el EMP. No contiene un indicador de cobertura poblacional ni un denominador territorial.', 'Las notas de codificación situadas bajo la tabla se excluyeron de los registros.'],
 '02':['La hoja codifica sexo como 0=Hombre y 1=Mujer. Las notas bajo la tabla se excluyeron.'],
 '03':['La percepción del paciente es ordinal. El diseño descrito no permite inferir causalidad de la formación.'],
 '04':['Todos los participantes del caso pertenecen a programas extracurriculares; no representan a toda la población universitaria.'],
 '05':['La hoja describe el puntaje CSS como mayor puntaje = mayor cibercondría. No se convirtió en diagnóstico clínico. Las notas al pie se excluyeron.'],
 '06.1':['El contexto menciona información MINSAL de 2018–2025 sin enlace ni licencia verificables. No se verificó si los registros son reales o simulados.','La edad está expresada en meses y alcanza 178,2 meses. No restringir el caso a lactantes por el título. El recuento por año no es incidencia sin denominador poblacional.','Las etiquetas de diagnóstico contienen variaciones de mayúsculas y tildes, conservadas de la fuente.'],
 '06.2':['El contexto menciona información MINSAL de 2018–2025 sin enlace ni licencia verificables. No se verificó si los registros son reales o simulados.','El recuento por año no es incidencia sin denominador poblacional.'],
 '07':['El contexto menciona casos MINSAL de 2025 sin enlace ni licencia verificables. No se verificó si los registros son reales o simulados.','La base no contiene identificador ni denominador poblacional. Los conteos describen la muestra, no incidencia.'],
 '08':['El contexto menciona casos MINSAL de 2024 sin enlace ni licencia verificables. No se verificó si los registros son reales o simulados.','La base no contiene identificador ni denominador poblacional. Los conteos describen la muestra, no incidencia.'],
 '09':['Se convirtieron las comas decimales de días de espera y distancia a valores numéricos. El diccionario situado a la derecha se excluyó de la tabla.'],
 '10':['El diccionario situado a la derecha se excluyó de la tabla. La pertenencia a pueblos originarios es un dato sensible.'],
 '11':['Hay 350 registros reales, aunque el área formateada de Excel se extiende hasta la fila 451.'],
 '12':['El escenario se refiere a adolescentes de 15–19 años y contiene información sensible sobre sexualidad. La columna edad corresponde al inicio de relaciones sexuales, no a edad actual.'],
 '13':['La clasificación de obesidad fue conservada tal como figura en la fuente. No se recalculó a partir del IMC. La fuente incluye grados I–III aunque el IMC observado abarca 30,9–37,7; la coherencia de estas variables requiere validación.'],
 '14':['Hay 349 registros, aunque el diccionario menciona folios hasta 420 y el área formateada llega a la fila 421. No se imputaron las filas vacías.'],
 '15':['Se preservó la codificación de la fuente para tipo de discapacidad visual y nivel de participación.'],
 '16':['La tabla no contiene parto prematuro ni edad gestacional. Permite describir factores y nivel de riesgo obstétrico, pero no estimar frecuencia ni factores asociados a parto prematuro.'],
 '17':['El resultado de evaluación psicomotora está codificado 1=Normal, 2=Riesgo, 3=Retraso.'],
 '18':['El nivel de sobrecarga está codificado 1=Ausente, 2=Leve, 3=Intensa.'],
 '19':['La variable ordinal describe adherencia a lactancia, con cuatro categorías. No hay fecha ni seguimiento longitudinal individual.'],
}

def slug(s):
 s=unicodedata.normalize('NFKD',str(s)).encode('ascii','ignore').decode().lower()
 return re.sub(r'[^a-z0-9]+','_',s).strip('_')

def valuelabels(s):
 matches=list(re.finditer(r'(?<![\w.])([0-9]+)\s*=\s*',s))
 out={}
 for i,m in enumerate(matches):
  txt=s[m.end():matches[i+1].start() if i+1<len(matches) else len(s)].strip(' ,.;')
  out[m.group(1)]=txt
 return out

SYNTHETIC_EVIDENCE = ('El responsable del curso confirmó el 25 de octubre de 2026 '
                      'que todas las bases son completamente ficticias o simuladas.')
# This confirmation updates provenance metadata; parsing and rows are unchanged.
NOTES = {number: [note.replace('registros reales', 'registros de datos')
                 for note in notes
                 if 'No se verificó si los registros son reales o simulados' not in note]
         for number, notes in NOTES.items()}

cases=[];trace=[]
for f in sorted(SOURCE.glob('*.xlsx'),key=lambda p:[float(v) for v in re.findall(r'\d+(?:\.\d+)?',p.name)[:1]]):
 number=re.search(r'(\d{2}(?:\.\d)?)',f.name).group(1)
 wb=load_workbook(f,read_only=True,data_only=True)
 sh=next(s for s in wb if 'base' in s.title.lower())
 values=list(sh.iter_rows(values_only=True)); headers=values[0]
 ncols=next((i for i,x in enumerate(headers) if x is None),len(headers))
 headers=[str(x).strip() for x in headers[:ncols]]
 has_id=headers[0].lower().startswith('id') or headers[0].lower()=='caso'
 table=[r[:ncols] for r in values[1:] if r[0] is not None]
 # Codebooks can be in a separate sheet, the case sheet, or at the table's right.
 dictionary={}
 for s in wb:
  for r in s.iter_rows(values_only=True):
   if s.title==sh.title:
    candidates=[(r[i],r[j]) for i in range(ncols,len(r)) for j in range(i+1,len(r)) if r[i] is not None and r[j] is not None]
   else:
    candidates=[(r[0],r[1])] if len(r)>1 and r[0] is not None and r[1] is not None else []
   for k,v in candidates:
    if str(k).strip()!='Nombre Variable': dictionary[str(k).strip()]=str(v).strip()
 columns=[]; keep=[]
 for i,label in enumerate(headers):
  if has_id and i==0:continue
  key=slug(label)
  # Avoid long coding text embedded in source headers.
  if number=='09': key=['id','dias_espera','distancia_km','n_consultas_urg','priorizado','sexo_biologico','macrozona','complica_previa','gravedad_sintoma'][i]
  dtype='numeric' if i in NUMERIC[number] else 'categorical'
  desc=dictionary.get(label,'')
  if not desc:
   desc=next((v for k,v in dictionary.items() if slug(k)==key),'')
  labs=valuelabels(desc or label)
  if number=='01' and i==5:
   labs={str(r[5]).split(':')[0].strip():str(r[5]).split(':',1)[1].strip() for r in values[301:] if r[5] is not None and ':' in str(r[5])}
  if number=='01' and i==6:labs={'1':'FONASA','2':'ISAPRE','3':'OTRA'}
  if number=='02' and i==2:labs={'0':'Hombre','1':'Mujer'}
  col={'key':key,'label':label,'type':dtype,'sourceColumn':i+1,'description':desc,'missing':sum(r[i] is None for r in table)}
  if i in ORDINAL[number]:col['ordered']=True
  if labs:col['valueLabels']=labs
  columns.append(col);keep.append(i)
 rows=[]
 for r in table:
  row={}
  for col,i in zip(columns,keep):
   v=r[i]
   if isinstance(v,str):v=v.strip()
   if col['type']=='numeric' and isinstance(v,str):v=float(v.replace(',','.'))
   if isinstance(v,float) and v.is_integer():v=int(v)
   row[col['key']]=v
  rows.append(row)
 for col in columns:
  vals=[r[col['key']] for r in rows if r[col['key']] is not None]
  col['distinct']=len(set(vals))
  if col['type']=='numeric':
   col['min']=min(vals) if vals else None;col['max']=max(vals) if vals else None
  else:col['values']=sorted(set(vals),key=lambda v:(str(type(v)),str(v)))
 context=' '.join(str(v) for s in wb if s.title!=sh.title for r in s.iter_rows(values_only=True) for v in r if isinstance(v,str))
 evidence='El contexto o diccionario del libro describe una muestra, encuesta o identificadores anonimizados.' if re.search(r'anonimiz',context,re.I) else 'El libro usa códigos de registro o categorías clínicas, sin declaración de anonimización ni fuente verificable.'
 if number in {'06.1','06.2','07','08'}:evidence='El contexto menciona información de MINSAL, sin fuente verificable en el libro ni declaración explícita de anonimización.'
 c={'id':'caso-'+number.replace('.','-'),'number':number,'title':TITLES[number],
  'sourceFile':f.name,'sheet':sh.title,'classification':'Datos simulados para docencia',
  'classificationEvidence':SYNTHETIC_EVIDENCE,'anonymization':'Identificadores seudónimos excluidos; no se hallaron nombres, RUT, correos, direcciones ni teléfonos en las tablas.',
  'n':len(rows),'columns':columns,'rows':rows,
  'dictionary':{col['key']:col['description'] for col in columns if col['description']},
  'notes':NOTES.get(number,[]),'excludedColumns':[headers[0]] if has_id else []}
 cases.append(c)
 trace.append({'id':c['id'],'sourcePath':str(f),'sourceFile':f.name,'sha256':hashlib.sha256(f.read_bytes()).hexdigest(),
  'sheet':sh.title,'excelMaxRow':sh.max_row,'excelMaxColumn':sh.max_column,'tableRows':len(table),
  'includedColumns':len(columns),'excludedColumns':c['excludedColumns'],'missingValues':sum(col['missing'] for col in columns)})

out={'generatedOn':'2026-10-25','scope':'20 casos docentes de Metodología de la Investigación DSPU0011; solo tablas para estudiantes',
 'dataNature':SYNTHETIC_EVIDENCE + ' No son estadísticas oficiales.',
 'transforms':['Se excluyeron identificadores seudónimos.','Se excluyeron hojas de preguntas/soluciones, nombres de estudiantes, notas, inscripciones y asistencia.',
 'Se excluyeron notas al pie y diccionarios laterales de las filas.','Se conservaron nulos y categorías originales; solo se recortaron espacios periféricos.',
 'Caso09: comas decimales convertidas a números.','Los códigos categóricos conservan su valor fuente y valueLabels proporciona etiquetas cuando existe diccionario.'],
 'cases':cases}
target=args.output.expanduser()
trace_target=args.trace.expanduser()
target.parent.mkdir(parents=True, exist_ok=True)
trace_target.parent.mkdir(parents=True, exist_ok=True)
target.write_text(json.dumps(out,ensure_ascii=False,indent=2,allow_nan=False), encoding='utf-8')
trace_target.write_text(json.dumps({'sourceRoot':str(SOURCE),'dataFile':str(target),'dataSha256':hashlib.sha256(target.read_bytes()).hexdigest(),'cases':trace},ensure_ascii=False,indent=2), encoding='utf-8')
print(json.dumps({'cases':len(cases),'records':sum(c['n'] for c in cases),'output':str(target),'trace':str(trace_target)}, ensure_ascii=False))
