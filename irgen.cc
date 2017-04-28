/* irgen.cc -  LLVM IR generator
 *
 * You can implement any LLVM related functions here.
 */

#include "irgen.h"

using namespace std;

IRGenerator::IRGenerator() :
    context(NULL),
    module(NULL),
    currentFunc(NULL),
    currentBB(NULL)
{
}

IRGenerator& IRGenerator::Instance() {
  static IRGenerator irgen;
  return irgen;
}

IRGenerator::~IRGenerator() {
}

llvm::Module *IRGenerator::GetOrCreateModule(const char *moduleID)
{
   if ( module == NULL ) {
     context = new llvm::LLVMContext();
     module  = new llvm::Module(moduleID, *context);
     module->setTargetTriple(TargetTriple);
     module->setDataLayout(TargetLayout);
   }
   return module;
}

void IRGenerator::SetFunction(llvm::Function *func) {
   currentFunc = func;
}

llvm::Function *IRGenerator::GetFunction() const {
   return currentFunc;
}

void IRGenerator::SetBasicBlock(llvm::BasicBlock *bb) {
   currentBB = bb;
}

llvm::BasicBlock *IRGenerator::GetBasicBlock() const {
   return currentBB;
}

llvm::BasicBlock *IRGenerator::CreateEmptyBlock(string name) {
  return llvm::BasicBlock::Create(*context, name, GetFunction());
}

llvm::Type *IRGenerator::GetIntType() const {
   llvm::Type *ty = llvm::Type::getInt32Ty(*context);
   return ty;
}

llvm::Type *IRGenerator::GetBoolType() const {
   llvm::Type *ty = llvm::Type::getInt1Ty(*context);
   return ty;
}

llvm::Type *IRGenerator::GetFloatType() const {
   llvm::Type *ty = llvm::Type::getFloatTy(*context);
   return ty;
}

llvm::Type *IRGenerator::GetVoidType() const {
   llvm::Type *ty = llvm::Type::getVoidTy(*context);
   return ty;
}

llvm::Type *IRGenerator::GetVec2Type() const {
  return llvm::VectorType::get(GetFloatType(), 2);
}

llvm::Type *IRGenerator::GetVec3Type() const {
  return llvm::VectorType::get(GetFloatType(), 3);
}

llvm::Type *IRGenerator::GetVec4Type() const {
  return llvm::VectorType::get(GetFloatType(), 4);
}

llvm::Constant *IRGenerator::GetConstantInt(int val) {
  return llvm::ConstantInt::get(GetIntType(), val, true);
}

llvm::Constant *IRGenerator::GetConstantFloat(float val) {
  return llvm::ConstantFP::get(GetFloatType(), val);
}

llvm::Value *IRGenerator::GetZeroVector(int size) {
  std::vector<llvm::Constant*> contents(size, GetConstantFloat(0.0));
  return llvm::ConstantVector::get(contents);
}

llvm::Value *IRGenerator::GetInsertInst(llvm::Value *vec, llvm::Value *elem, int idx) {
  // llvm::Value *index = llvm::ConstantInt::get(GetIntType(), idx, true);
  return llvm::InsertElementInst::Create(vec, elem, llvm::ConstantInt::get(GetIntType(), idx, true), "", GetBasicBlock());
}

llvm::Value *IRGenerator::GetExtractInst(llvm::Value *vec, int idx) {
  // llvm::Value *index = llvm::ConstantInt::get(GetIntType(), idx, true);
  return llvm::ExtractElementInst::Create(vec, llvm::ConstantInt::get(GetIntType(), idx, true), "", GetBasicBlock());
}

llvm::Value *IRGenerator::checkLLVMvec(llvm::Value *value, int size) {
  if (value->getType()->isVectorTy())
    return value;

  // cerr << "inside checkLLVMvec " <<endl;
  llvm::Value *vec = GetZeroVector(size);
  for (int i = 0; i < size; ++i){
     vec = GetInsertInst(vec, value, i);
  }

   // cerr << "inside checkLLVMvec " <<endl;
  return vec;
}

llvm::Value *IRGenerator::checkVecAndBool(llvm::Value *vector) {
  IRGenerator &irgen = IRGenerator::Instance();

  llvm::Value* idx = llvm::ConstantInt::get(GetIntType(), 0, true);

  // cerr << "in checkVecAndBool calling ExtractElementInst "<<endl;

  llvm::Value *andResult = llvm::ExtractElementInst::Create(vector, llvm::ConstantInt::get(GetIntType(), 0, true), "", GetBasicBlock());
    //llvm::ExtractElementInst::Create(vec, index, "", GetBasicBlock());

  for (int i = 1; i < vector->getType()->getVectorNumElements(); ++i) {
    llvm::Value *elem = llvm::ExtractElementInst::Create(vector, llvm::ConstantInt::get(GetIntType(), i, true), "", GetBasicBlock());
    andResult = llvm::BinaryOperator::CreateAnd(andResult, elem, "", irgen.GetBasicBlock());
  }

  return andResult;
}

llvm::Value *IRGenerator::PostFixIncrementInst(llvm::Value *value) {
  llvm::Type *valType = value->getType();
  bool is_float_op = valType->isFloatTy() || valType->isVectorTy();
  llvm::BinaryOperator::BinaryOps op;
  int scalar = 1;

  llvm::Value *scalarVal;
  if (is_float_op){
    op = llvm::BinaryOperator::FAdd;
    scalarVal = llvm::ConstantFP::get(GetFloatType(), scalar);
  } 
  else{
    op = llvm::BinaryOperator::Add;
    scalarVal = llvm::ConstantInt::get(GetIntType(), (int)scalar, true);
  }

  return llvm::BinaryOperator::Create(op, value, scalarVal, "", GetBasicBlock());
}

llvm::Value *IRGenerator::PostFixDecrementInst(llvm::Value *value) {
  llvm::Type *valType = value->getType();
  bool is_float_op = valType->isFloatTy() || valType->isVectorTy();
  llvm::BinaryOperator::BinaryOps op;
  int scalar = 1;

  llvm::Value *scalarVal;
  if (is_float_op){
    op = llvm::BinaryOperator::FSub;
    scalarVal = llvm::ConstantFP::get(GetFloatType(), scalar);
  } 
  else{
    op = llvm::BinaryOperator::Sub;
    scalarVal = llvm::ConstantInt::get(GetIntType(), (int)scalar, true);
  }

  return llvm::BinaryOperator::Create(op, value, scalarVal, "", GetBasicBlock());
}

const char *IRGenerator::TargetLayout = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64-S128";

const char *IRGenerator::TargetTriple = "x86_64-redhat-linux-gnu";

