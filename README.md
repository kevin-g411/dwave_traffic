ここには量子コンピュータ「D-Wave Leap」を用いた渋滞回避経路割り振りソフトを掲載してます。

大まかな構造としては、
  1. UIとなる入出力の画面（エクセル）
  2. パラメータを受け取りd-waveに接続し結果を受け取る処理（python）
があります。

拡張子のないものはエクセル内のマクロを（見やすいかなと思い）掲載しました。
名前にModuleとつくものは独立するマクロで、
つかないものはEntireModule内の個別のマクロです。（EntireModuleと中身は重複している）

構造としては
xlwingsModule
Module1
┠start（「準備」ボタンで実行：表などを作成）
Module4
┠exe（「実行」ボタンで実行：下のマクロを順に実行）
┠save
┠calc
┠python_exe
┠output
